!     Last change:  AA    6 Dec 101    9:05 am
!_________________________________________________________________________________________
function CloseFile(unitnum)
!  This subroutine closes the files that were opened by the scripts.
!  The unit is supplied by the scripts based on the unit used in MODFLOW.
!     fileopend boolean to test if file is opened.
!     Unitnum   unit to close
!     error flags should return error if file is not opened yet the script expects it to be.
! ************************************************************
! Written By: Patrick Tara
! Modifications List
! by            When     description
!  


      IMPLICIT NONE
      integer*4 unitnum
      integer*4 CloseFile
      logical fileopened

      DLL_EXPORT CloseFile

      write(*,*)'unit to close:',unitnum
      inquire(unit=unitnum,opened=fileopened)

      if (fileopened) then
	   close(unit=unitnum)
      endif

      CloseFile=0
      return
      end
!_________________________________________________________________________________________
function Read_MODFLOW_Unformatted(xArray,nCol,nRow,layNo,infSource,infType,infUnit,infName,outfType,outfUnit,outfName)
! ************************************************************
! Reads Unformatted Files (Head, Drawdown, or Cell-by-Cell Flow) Written by MODFLOW
! and stores one in xArray.
!       xArray                  Array to be filled by this function
!       nCol                    Number of Columns in the Array
!       nRow                    Number of Rows in the Array
!       layNo                   Layer Number for Array (vertical coordinate for array)
!	infSource		Input File Source: (1=MODFLOW Head, 2=MT3D Concentration, 3=MODFLOW CBC Flow)
!       infType			Input File Type: (0=True Binary, 1=Unformatted)
!       infUnit			Unit Number to be Used for Opening the Input File
!	infName			Input File name
!       outfType		Output File Type: (-1=no output file, 0=Binary, 1=Unformatted, 2=ASCII Array, 3=ASCII R,C,L,Val)
!       outfUnit		Unit Number to be Used for Opening the Output File (used only when outfType is not -1)
!	outfName		Output File name (used only when outfType is not -1)
! ************************************************************
! Written By: Alaa Aly
! Modifications List
! by            When     description
! Patrick Tara  3/10/02  Reversed the row and columns on the array for compatibility with the vb codes
! Patrick Tara  3/11/02  Do not close file, File will be closed at the end of the simulation

      IMPLICIT NONE
      INTEGER*4      Read_MODFLOW_Unformatted
      REAL*4         xArray
      INTEGER*4      nCol,nRow,layNo,infSource,infType,infUnit,outfType,outfUnit
      DIMENSION      xArray(Nrow,Ncol)
      CHARACTER*255  INFNAME,OUTFNAME

      CHARACTER*16   TEXT
      INTEGER*4      IC,IR,IL,ILAY,nColFile,nRowFile,NLAY,NTRANS,NSPER,NTSTEP
      INTEGER*4      IERR,KSTP,KPER,ERRFLAG
      REAL*4         TOTIME,PERTIME,BUFF3
      DIMENSION      BUFF3(:,:,:)
      ALLOCATABLE :: BUFF3
      logical        fileopened

      DLL_EXPORT Read_MODFLOW_Unformatted

      Read_MODFLOW_Unformatted = 0

1001  FORMAT(' READING ',A16,' T.STEP ',I3,' S.PERIOD ',I3,' LAYER ',I2,' Ncol=',I4,' Nrow=',I4)
1002  FORMAT(' READING ',A16,' T.STEP ',I3,' S.PERIOD ',I3,' Nlay ',I2,' Ncol=',I4,' Nrow=',I4)
1003  FORMAT(' LENGTH OF STRESS PERIOD = ',F10.2,'  TOTAL TIME ELAPSED = ',F10.2)
1004  FORMAT(' Reading Array...... ')
1005  FORMAT(' Finished Reading Array. ')

2001  FORMAT(' READING ',A16,'TransStep ',I3,' T.STEP ',I3,' S.PERIOD ',I3,' LAYER ',I2,' Ncol=',I4,' Nrow=',I4)
2003  FORMAT('  TOTAL TIME ELAPSED = ',F10.2)


3000  FORMAT(3I10,G30.8)
3010  FORMAT(30000E12.4)
3100  FORMAT(5I15)
3200  FORMAT(I10)
3300  FORMAT(2I10,2F10.3,A16,3I10)
3301  FORMAT(3I10,F10.3,A16,3I10)

!      write(*,*)'ncol:',nCol,' nrow:',nRow,' layno:',layNo
!      write(*,*)'infsource:',infSource,' inftype:',infType,' infunit:',infUnit
!      write(*,*)'infName:',infName
!      write(*,*)'outfType:',outfType,' outfUnit:',outfUnit
!      write(*,*)'outfName:',outfName

      inquire(unit=infunit,opened=fileopened)

829   continue

      if(INFTYPE .eq. 0) then  ! input file is binary
           WRITE(*,*)'Opening Input Binary File: ',INFNAME
           OPEN(UNIT=INFUNIT,FILE=INFNAME,STATUS='OLD',ERR=1929,ACCESS='TRANSPARENT',ACTION='DENYNONE')
      else if(INFTYPE .eq. 1) then   ! input file is unformatted
           if (fileopened) then
              write(*,*)'file already opened'
           else
              WRITE(*,*)'Opening Input Unformatted File: ',INFNAME
              OPEN(UNIT=INFUNIT,FILE=INFNAME,STATUS='OLD',ERR=1929,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',ACTION='DENYNONE')
           endif
      endif


      if(OUTFTYPE .eq. 0) then   ! output file is binary
           WRITE(*,*)'Opening Output Binary File: ',OUTFNAME
           OPEN(UNIT=OUTFUNIT,FILE=OUTFNAME,STATUS='REPLACE',ACCESS='TRANSPARENT',ACTION='WRITE')
      else if(OUTFTYPE .eq. 1) then  ! output file is unformatted
           WRITE(*,*)'Opening Output Unformatted File: ',OUTFNAME
           OPEN(UNIT=OUTFUNIT,FILE=OUTFNAME,STATUS='REPLACE',ACCESS='SEQUENTIAL',FORM='UNFORMATTED',ACTION='WRITE')
      else if(OUTFTYPE .eq. 2 .or. OUTFTYPE .eq. 3) then ! OUTFYPE = 2 ==> output file is ASCII -- arrays
                                                         ! OUTFYPE = 3 ==> output file is ASCII -- (row,col,layer,value)
           WRITE(*,*)'Opening Output ASCII File: ',OUTFNAME
           OPEN(UNIT=OUTFUNIT,FILE=OUTFNAME,STATUS='REPLACE',ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE')
      endif

      NLAY   = 0 ! initialize variables
      NSPER  = 0
      NTSTEP = 0

1700  continue
      if(INFSOURCE .eq. 1) READ(INFUNIT,err=1930,end=1933) KSTP,KPER,PERTIME,TOTIME,TEXT,nColFile,nRowFile,ILAY ! READ IDENTIFYING RECORD
      if(INFSOURCE .eq. 2) READ(INFUNIT,err=1930,end=1933) NTRANS,KSTP,KPER,TOTIME,TEXT,nColFile,nRowFile,ILAY
      if(INFSOURCE .eq. 3) READ(INFUNIT,err=1930,end=1933) KSTP,KPER,TEXT,nColFile,nRowFile,NLAY

      ! check if file contains the expected information
      IF(infSource .eq.  3 .AND. nLay .lt. layNo) GOTO 1917
      IF(nCol .NE. nColFile) goto 1918
      IF(nRow .NE. nRowFile) goto 1919

      WRITE(*,*) ! write blank line
      if(INFSOURCE .eq. 1) then
         WRITE(*,1001)TEXT,KSTP,KPER,ILAY,NCOL,NROW
         WRITE(*,1003)PERTIME,TOTIME
      else if(INFSOURCE .eq. 2) then
         WRITE(*,2001)TEXT,NTRANS,KSTP,KPER,ILAY,NCOL,NROW
         WRITE(*,2003)TOTIME
      else if(INFSOURCE .eq. 3) then
         WRITE(*,1002)TEXT,KSTP,KPER,NLAY,NCOL,NROW
         WRITE(*,1003)PERTIME,TOTIME
      endif

      if(NROW .le. 0) goto 1920
      if(NCOL .le. 0) goto 1921
      if((INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) .and. (ILAY .le. 0)) goto 1922
      if(INFSOURCE .eq. 3 .and. NLAY .le. 0) goto 1923

      if(INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) NLAY   = max(NLAY,ILAY)
      NSPER  = max(NSPER,KPER)
      NTSTEP = max(NTSTEP,KSTP)

!      if(INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) ====> no array allocation is needed since xArray will be used
      if(INFSOURCE .eq. 3) ALLOCATE (BUFF3(NCOL,NROW,NLAY),STAT=IERR)
      IF(IERR.NE.0) goto 1931 ! if memory is not allocated: report an error and quit

      if((INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) .and. INFTYPE .eq. 0) then ! binary input file
         DO IR=1,NROW
            DO IC=1,NCOL
               READ(INFUNIT,err=1930)xArray(IR,IC)
               write(*,*) IR,IC,xArray(IR,IC)
            END DO
         END DO
      else if(INFSOURCE .eq. 3 .and. INFTYPE .eq. 0) then ! binary 3D file
         DO IL=1,NLAY
            DO IR=1,NROW
               DO IC=1,NCOL
                   READ(INFUNIT,err=1930) BUFF3(IC,IR,IL)
               END DO
            END DO
         END DO
      else if((INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) .and. INFTYPE .eq. 1) then  ! unformatted input file
         READ(INFUNIT,err=1930)((xArray(IR,IC),IC=1,NCOL),IR=1,NROW)
      else if(INFSOURCE .eq. 3 .and. INFTYPE .eq. 1) then  ! unformatted 3D input file
         READ(INFUNIT,err=1930)(((BUFF3(IC,IR,IL),IC=1,NCOL),IR=1,NROW),IL=1,NLAY)
         do IC=1,NCOL
            do IR=1,NROW
               xArray(IR,IC) = BUFF3(IC,IR,layNo)
            END do
         END do
      endif

      ! write output file, only if outfType is not -1
      if((INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) .and. OUTFTYPE .eq. 0) then ! binary output file
         if(INFSOURCE .eq. 1) WRITE(OUTFUNIT) KSTP,KPER,PERTIME,TOTIME,TEXT,NCOL,NROW,ILAY
         if(INFSOURCE .eq. 2) WRITE(OUTFUNIT) NTRANS,KSTP,KPER,TOTIME,TEXT,NCOL,NROW,ILAY
         DO IR=1,NROW
            DO IC=1,NCOL
               WRITE(OUTFUNIT)xArray(IR,IC)
            END DO
         END DO
      else if(INFSOURCE .eq. 3 .and. OUTFTYPE .eq. 0) then ! binary 3D output file
         WRITE(OUTFUNIT) KSTP,KPER,PERTIME,TOTIME,TEXT,NCOL,NROW,NLAY
         DO IL=1,NLAY
            DO IR=1,NROW
               DO IC=1,NCOL
                  WRITE(OUTFUNIT)BUFF3(IC,IR,IL)
               END DO
            END DO
         END DO
      else if(OUTFTYPE .eq. 1) then ! unformatted output file
         if(INFSOURCE .eq. 1) WRITE(OUTFUNIT) KSTP,KPER,PERTIME,TOTIME,TEXT,NCOL,NROW,ILAY
         if(INFSOURCE .eq. 2) WRITE(OUTFUNIT) NTRANS,KSTP,KPER,TOTIME,TEXT,NCOL,NROW,ILAY
         if(INFSOURCE .eq. 3) WRITE(OUTFUNIT) KSTP,KPER,PERTIME,TOTIME,TEXT,NCOL,NROW,NLAY
         if(INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) WRITE(OUTFUNIT)((xArray(IR,IC),IC=1,NCOL),IR=1,NROW)
         if(INFSOURCE .eq. 3) WRITE(OUTFUNIT)(((BUFF3(IC,IR,IL),IC=1,NCOL),IR=1,NROW),IL=1,NLAY)
      else if(OUTFTYPE .eq. 2) then ! ASCII matrix output file
         WRITE(OUTFUNIT,*) ! write blank line
         if(INFSOURCE .eq. 1) WRITE(OUTFUNIT,3300) KSTP,KPER,PERTIME,TOTIME,TEXT,NCOL,NROW,ILAY
         if(INFSOURCE .eq. 2) WRITE(OUTFUNIT,3301) NTRANS,KSTP,KPER,TOTIME,TEXT,NCOL,NROW,ILAY
         if(INFSOURCE .eq. 3) WRITE(OUTFUNIT,3300) KSTP,KPER,PERTIME,TOTIME,TEXT,NCOL,NROW,NLAY
         if(INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) then
            DO IR=1,NROW
               WRITE(OUTFUNIT,3010) (xArray(IR,IC),IC=1,NCOL)
            END DO
         else if(INFSOURCE .eq. 3) then
            DO IL=1,NLAY
               DO IR=1,NROW
                  WRITE(OUTFUNIT,3010) (BUFF3(IC,IR,IL),IC=1,NCOL)
               END DO
            END DO
         endif
      else if((INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) .and. (OUTFTYPE .eq. 3)) then ! ASCII (row,col,layer,value) output file
         DO IR=1,NROW
            DO IC=1,NCOL
               WRITE(OUTFUNIT,3000) IR,IC,ILAY,xArray(IR,IC)
            END DO
         END DO
      else if(INFSOURCE .eq. 3 .and. outfType .eq. 3) then ! ASCII (row,col,layer,value) output file
         DO IL=1,NLAY
            DO IR=1,NROW
               DO IC=1,NCOL
                  WRITE(OUTFUNIT,3000) IR,IC,IL,BUFF3(IC,IR,IL)
               END DO
            END DO
         END DO
      endif

!      if(INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) ====> no de-allocation is needed
      if(infSource .eq. 3) DEALLOCATE (BUFF3)
      IF((infSource .eq. 1 .OR. infSource .eq. 2) .AND. iLay .eq. layNo) GOTO 1933
      goto 1700
1917  continue
      ERRFLAG = -140   ! Error: nLay is smaller than layNo
      write(*,*)' Error: invalid layNo'
      goto 1933
1918  continue
      ERRFLAG = -130   ! Error: nCol is not equal to nColFile
      write(*,*)' Error: nCol does not match binary file. NCOL:',ncol,',NCOLFILE:',ncolfile
      goto 1933
1919  continue
      ERRFLAG = -120   ! Error: nRow is not equal to nRowFile
      write(*,*)' Error: nRow does not match binary file. NROW:',nrow,',NROWFILE:',nrowfile
      goto 1933
1920  continue
      ERRFLAG = -110   ! Error: invalid NROW
      write(*,*)' Error: invalid NROW. NROW:',nrow,',NROWFILE:',nrowfile
      goto 1933
1921  continue
      ERRFLAG = -109   ! Error: invalid NCOL
      write(*,*)' Error: invalid NCOL. NCOL:',ncol,',NCOLFILE:',ncolfile
      goto 1933
1922  continue
      ERRFLAG = -108   ! Error: invalid ILAY
      write(*,*)' Error: invalid ILAY'
      goto 1933
1923  continue
      ERRFLAG = -107   ! Error: invalid NLAY
      write(*,*)' Error: invalid NLAY'
      goto 1933
1929  continue
      ERRFLAG = -1   ! Error: in opening the input file
      write(*,*)' Error: in opening the input file'
      goto 1933
1930  continue
      ERRFLAG = 1    ! Error: in reading the input file
      write(*,*)' Error: in reading the input file'
      goto 1933
1931  continue
      ERRFLAG = 2    ! NOT ENOUGH MEMORY
      write(*,*)' Error: NOT ENOUGH MEMORY'

1933  continue

!      CLOSE(UNIT=INFUNIT)
!      IF(outfType .ge. 0) CLOSE(UNIT=OUTFUNIT)

      Read_MODFLOW_Unformatted = ERRFLAG
      return
      END
!_________________________________________________________________________________________
function Read_MODFLOW_List(xArray,layArray,colArray,rowArray,nCol,nRow,nCells,infType,infUnit,infName,outfType,outfUnit,outfName)
! ************************************************************
! Reads List Unformatted Files Written by MODFLOW-96 (subroutine UBDSVA)
! and stores it in xArray and stores location in layArray, rowArray, and colArray.
!       xArray                  Array to be filled by this function
!       nCol                    Number of Columns in the Array
!       nRow                    Number of Rows in the Array
!	infSource		Input File Source. Used only for Identification Purposes (1=MODFLOW River, 2=MODFLOW Drain, 3=MODFLOW Stream, 4=MODFLOW GHB, 5=MOFLOW Well)
!       infType			Input File Type: (0=True Binary, 1=Unformatted)
!       infUnit			Unit Number to be Used for Opening the Input File
!	infName			Input File name
!       outfType		Output File Type: (-1=no output file, 0=Binary, 1=Unformatted, 2=ASCII)
!       outfUnit		Unit Number to be Used for Opening the Output File (used only when outfType is not -1)
!	outfName		Output File name (used only when outfType is not -1)
! ************************************************************
! Written By: Alaa Aly
! Modifications List
! by            When     description
! Alaa Aly     11-29-01
! Patrick Tara  3/11/02  Do not close file, File will be closed at the end of the simulation
      IMPLICIT NONE
      INTEGER*4      Read_MODFLOW_List
      REAL*4         xArray,layArray,colArray,rowArray
      INTEGER*4      nCol,nRow,nCells,infSource,infType,infUnit,outfType,outfUnit
      DIMENSION      xArray(nCells),layArray(nCells),colArray(nCells),rowArray(nCells)
      CHARACTER*255  INFNAME,OUTFNAME

      CHARACTER*16   TEXT
      INTEGER*4      IC,IR,IL,ILAY,nColFile,nRowFile,NLAY,NTRANS,NSPER,NTSTEP,NCellsFile,fileFlag
      INTEGER*4      IERR,KSTP,KPER,ERRFLAG,icell,icrl
      REAL*4         TOTIME,PERTIME,DELT,PERTIM,TOTIM,Q

      DLL_EXPORT Read_MODFLOW_List

      Read_MODFLOW_List = 0

1002  FORMAT(' READING ',A16,' T.STEP ',I3,' S.PERIOD ',I3,' Nlay ',I2,' Ncol=',I4,' Nrow=',I4)
1003  FORMAT(' LENGTH OF STRESS PERIOD = ',F10.2,'  TOTAL TIME ELAPSED = ',F10.2)
1004  FORMAT(' Reading Array...... ')
1005  FORMAT(' Finished Reading Array. ')

2001  FORMAT(' READING ',A16,'TransStep ',I3,' T.STEP ',I3,' S.PERIOD ',I3,' LAYER ',I2,' Ncol=',I4,' Nrow=',I4)
2003  FORMAT('  TOTAL TIME ELAPSED = ',F10.2)

3000  FORMAT(2I5,A16,3I5)
3010  FORMAT(I5,3G30.6)
3020  FORMAT(I5)
3030  FORMAT(I20,G30.8)

829   continue

      if(INFTYPE .eq. 0) then  ! input file is binary
           WRITE(*,*)'Opening Input Binary File: ',INFNAME
           OPEN(UNIT=INFUNIT,FILE=INFNAME,STATUS='OLD',ERR=1929,ACCESS='TRANSPARENT',ACTION='DENYNONE')
      else if(INFTYPE .eq. 1) then   ! input file is unformatted
           WRITE(*,*)'Opening Input Unformatted File: ',INFNAME
           OPEN(UNIT=INFUNIT,FILE=INFNAME,STATUS='OLD',ERR=1929,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',ACTION='DENYNONE')
      endif

      if(OUTFTYPE .eq. 0) then   ! output file is binary
           WRITE(*,*)'Opening Output Binary File: ',OUTFNAME
           OPEN(UNIT=OUTFUNIT,FILE=OUTFNAME,STATUS='REPLACE',ACCESS='TRANSPARENT',ACTION='WRITE')
      else if(OUTFTYPE .eq. 1) then  ! output file is unformatted
           WRITE(*,*)'Opening Output Unformatted File: ',OUTFNAME
           OPEN(UNIT=OUTFUNIT,FILE=OUTFNAME,STATUS='REPLACE',ACCESS='SEQUENTIAL',FORM='UNFORMATTED',ACTION='WRITE')
      else if(OUTFTYPE .eq. 2 .or. OUTFTYPE .eq. 3) then ! OUTFYPE = 2 ==> output file is ASCII
           WRITE(*,*)'Opening Output ASCII File: ',OUTFNAME
           OPEN(UNIT=OUTFUNIT,FILE=OUTFNAME,STATUS='REPLACE',ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE')
      endif

! initialize variables
      NSPER  = 0
      NTSTEP = 0

1700  continue
! READ IDENTIFYING RECORD
      READ(INFUNIT,err=1930,end=1933) KSTP,KPER,TEXT,nColFile,nRowFile,NLAY
      READ(INFUNIT,err=1930,end=1933) fileFlag,DELT,PERTIM,TOTIM
      READ(INFUNIT,err=1930,end=1933) NCellsFile

      NLAY = -NLAY
      ! check if file contains the expected information
      if(nCells .NE. nCellsFile) goto 1917
      if(nCol .NE. nColFile) goto 1918
      if(nRow .NE. nRowFile) goto 1919
      if(NROW .le. 0) goto 1920
      if(NCOL .le. 0) goto 1921
      if(NLAY .le. 0) goto 1923

      NSPER  = max(NSPER,KPER)
      NTSTEP = max(NTSTEP,KSTP)

      ! write output file, only if outfType is not -1
      if(OUTFTYPE .eq. 0 .OR. OUTFTYPE .eq. 1) then ! output file is binary or unformatted
          WRITE(outfUnit) KSTP,KPER,TEXT,nColFile,nRowFile,NLAY
          WRITE(outfUnit) fileFlag,DELT,PERTIM,TOTIM
          WRITE(outfUnit) NCellsFile
      else if(OUTFTYPE .eq. 2) then ! ASCII
          WRITE(outfUnit,3000) KSTP,KPER,TEXT,nColFile,nRowFile,NLAY
          WRITE(outfUnit,3010) fileFlag,DELT,PERTIM,TOTIM
          WRITE(outfUnit,3020) NCellsFile
      endif

      WRITE(*,*) ! write blank line
      WRITE(*,1002)TEXT,KSTP,KPER,NLAY,NCOL,NROW
      WRITE(*,1003)PERTIME,TOTIME

      if(INFSOURCE .eq. 1 .or. INFSOURCE .eq. 2) NLAY   = max(NLAY,ILAY)
      NSPER  = max(NSPER,KPER)
      NTSTEP = max(NTSTEP,KSTP)

! Finally, Ready To Do The Real Work: Read Cell-By-Cell Flows
      DO icell=1,Ncells
         READ(infUnit,err=1930) icrl,Q
         layArray(icell) = INT(icrl/(nCol*nRow)) + 1
         icrl = icrl - (layArray(icell)-1)*nCol*nRow
         rowArray(icell) =  INT(icrl/nCol) + 1
         colArray(icell) = icrl - (rowArray(icell)-1)*nCol
         xArray(icell) = Q
         ! write output file, only if outfType is not -1
         if(OUTFTYPE .eq. 0 .OR. OUTFTYPE .eq. 1) then ! output file is binary or unformatted
             WRITE(outfUnit,err=1930) icrl,Q
         else if(OUTFTYPE .eq. 2) then ! ASCII
             WRITE(outfUnit,3030,err=1930) icrl,Q
         endif
      END DO

      goto 1700

1917  continue
      ERRFLAG = -140   ! Error: nCells is not equal to nCellsFile
      write(*,*)' Error: invalid layNo'
      goto 1933
1918  continue
      ERRFLAG = -130   ! Error: nCol is not equal to nColFile
      write(*,*)' Error: invalid nCol'
      goto 1933
1919  continue
      ERRFLAG = -120   ! Error: nRow is not equal to nRowFile
      write(*,*)' Error: invalid nRow'
      goto 1933
1920  continue
      ERRFLAG = -110   ! Error: invalid NROW
      write(*,*)' Error: invalid NROW'
      goto 1933
1921  continue
      ERRFLAG = -109   ! Error: invalid NCOL
      write(*,*)' Error: invalid NCOL'
      goto 1933
1922  continue
      ERRFLAG = -108   ! Error: invalid ILAY
      write(*,*)' Error: invalid ILAY'
      goto 1933
1923  continue
      ERRFLAG = -107   ! Error: invalid NLAY
      write(*,*)' Error: invalid NLAY'
      goto 1933
1929  continue
      ERRFLAG = -1   ! Error: in opening the input file
      write(*,*)' Error: in opening the input file'
      goto 1933
1930  continue
      ERRFLAG = 1    ! Error: in reading the input file
      write(*,*)' Error: in reading the input file'
      goto 1933
1931  continue
      ERRFLAG = 2    ! NOT ENOUGH MEMORY
      write(*,*)' Error: NOT ENOUGH MEMORY'

1933  continue

!      CLOSE(UNIT=INFUNIT)
!      IF(outfType .ge. 0) CLOSE(UNIT=OUTFUNIT)

      Read_MODFLOW_List = ERRFLAG
      return
      END
!_________________________________________________________________________________________

!_________________________________________________________________________________________
function Read_Integer_Array(IA,ANAME,II,JJ,K,infUnit,infName)
! ************************************************************
! THIS SUBROUTINE IS USED TO INPUT 1 OR 2D INTEGER ARRAYS
! BY BLOCK, ZONAL, LIST-DIRECTED, UNFORMATTED,
! OR ANY USER-SPECIFIED FORMAT.
! IA     	Integer Array to be Read from File.
! ANAME         Array Name. Only Used for Identification Purposes.
! II            Number of Rows in Array.
! JJ            Number of Columns in Array.
! K             Layer Number. Only Used for Identification Purposes.
! infUnit       Input File Unit Number.
! infName       Input File Name.
! ********************************************************
! Modified from U2DINT in MCDONALD AND HARBAUGH (1988) as Modified in Zheng (1992).
! Written By: Alaa Aly
! last modified: 11-29-01

      IMPLICIT  NONE
      INTEGER   Read_Integer_Array
      INTEGER   IA,II,JJ,K,infUnit,IZV,IREAD,ICONST,IPRN,NBLOCK,NZONES,I1,I2,J1,J2,IZ,NN,I,J,N,IERR
      LOGICAL   OPD
      CHARACTER ANAME*24,FMTIN*20,infName*50
      DIMENSION IA(JJ,II),IZV(:)
      ALLOCATABLE :: IZV

      DLL_EXPORT Read_Integer_Array

      Read_Integer_Array = 0

! Check if input file is open
      IF(infUnit .eq. 0) GOTO 468
      INQUIRE(UNIT=IABS(infUnit), OPENED = OPD)
      IF(.NOT. OPD) THEN
         IF(infUnit .gt. 0) OPEN(UNIT=IABS(infUnit),FILE=infName,STATUS='OLD',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
         IF(infUnit .lt. 0) OPEN(UNIT=IABS(infUnit),FILE=infName,STATUS='OLD',ERR=469,ACCESS='TRANSPARENT',ACTION='READ')
      ENDIF
! Make infUnit ready for usage
      infUnit = iabs(infUnit)
!
!--READ ARRAY CONTROL RECORD
!  =========================
      READ (infUnit,1) IREAD,ICONST,FMTIN,IPRN
    1 FORMAT(I10,I10,A20,I10)

!
!--IF IREAD=0, SET ALL ARRAY VALUES EQUAL TO ICONST.
!  =================================================
      IF(IREAD.NE.0) GOTO 50
!
      DO 10 I=1,II
        DO 12 J=1,JJ
          IA(J,I)=ICONST
   12   CONTINUE
   10 CONTINUE

      GOTO 500
!
!--IF IREAD=100, INPUT ARRAY USING FORMAT FMTIN
!  ============================================
   50 IF(IREAD.NE.100) GOTO 90
!
      DO 30 I=1,II
        READ (infUnit,FMTIN) (IA(J,I),J=1,JJ)
   30 CONTINUE
      GOTO 300
!
!--IF IREAD=101, INPUT ARRAY USING BLOCK FORMAT
!  ============================================
   90 IF(IREAD.NE.101) GOTO 100
!
!
!--READ NUMBER OF BLOCKS
      READ(infUnit,*) NBLOCK
!
!--READ VALUE OF EACH BLOCK
!--AND ASSIGN VALUE TO CELLS WITHIN THE BLOCK
      DO 70 N=1,NBLOCK
        READ(infUnit,*) I1,I2,J1,J2,IZ
        DO 72 I=I1,I2
          DO 74 J=J1,J2
            IA(J,I)=IZ
   74     CONTINUE
   72   CONTINUE
   70 CONTINUE
      GOTO 300
!
!--IF IREAD=102, INPUT ARRAY USING ZONAL FORMAT
!  ============================================
  100 IF(IREAD.NE.102) GOTO 200
!
!--READ NUMBER OF ZONES
      READ(infUnit,*) NZONES
! alaa - canceled this in favor of dynamic memory allocation
!      IF(NZONES.GT.NZMAX) THEN
!      STOP
!      ENDIF
      ALLOCATE (IZV(NZONES),STAT=IERR)
      IF(IERR.NE.0) goto 470 ! if memory is not allocated: report an error and quit
! alaa - end of change

!
!--READ ZONAL MAP WITH FORMAT FMTIN
      READ(infUnit,*) (IZV(N),N=1,NZONES)
      DO 175 I=1,II
        READ(infUnit,FMTIN) (IA(J,I),J=1,JJ)
  175 CONTINUE
!
!--ASSIGN ZONAL VALUES
      DO 176 I=1,II
        DO 177 J=1,JJ
          NN=IA(J,I)
          IF(NN.EQ.0) THEN
            IA(J,I)=0
          ELSE
            IA(J,I)=IZV(NN)
          ENDIF
  177   CONTINUE
  176 CONTINUE
      DEALLOCATE(IZV)
      GOTO 300
!
!--IF IREAD=103, INPUT ARRAY USING FREE FORMAT
!  ===========================================
  200 IF(IREAD.NE.103) GOTO 250
!
!--READ ARRAY VALUES WITH FREE FORMAT
      READ(infUnit,*) ((IA(J,I),J=1,JJ),I=1,II)
      GOTO 300
!
!--IF IREAD IS EQUAL TO ANY OTHER VALUES,
!--READ ARRAY VALUES FROM AN EXTERNAL FILE ON UNIT [IREAD]
!  =======================================================
!
!--CHECK IF THE EXTERNAL FILE HAS BEEN OPENED.  IF NOT, OPEN
  250 INQUIRE(UNIT=IABS(IREAD), OPENED = OPD)
      IF(.NOT. OPD) THEN
         IF(IREAD .gt. 0) then
             OPEN(UNIT=IABS(IREAD),FILE=infName,STATUS='OLD',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
             DO I=1,II
                READ (IREAD,FMTIN) (IA(J,I),J=1,JJ)
             END DO
         ENDIF
         IF(IREAD .lt. 0) then
             OPEN(UNIT=IABS(IREAD),FILE=infName,STATUS='OLD',ERR=469,ACCESS='TRANSPARENT',ACTION='READ')
             READ(IABS(IREAD))
             READ(IABS(IREAD)) IA
         ENDIF
      ENDIF
      CLOSE(IABS(IREAD))

!
!--IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
  300 IF(ICONST.EQ.0) GO TO 487
      DO 302 I=1,II
      DO 304 J=1,JJ
        IA(J,I)=IA(J,I)*ICONST
  304 CONTINUE
  302 CONTINUE

      GOTO 487
  468 Read_Integer_Array = -11
      RETURN
  469 Read_Integer_Array = -10
      RETURN
  470 Read_Integer_Array = -1
      RETURN
  487 continue
      Read_Integer_Array = 1
  500 RETURN
      END
!
!____________________________________________________________________________________
function Read_Real_Array(A,ANAME,II,JJ,K,infUnit,infName)
! ********************************************************
! THIS SUBROUTINE IS USED TO INPUT 1 OR 2D REAL ARRAYS,
! BY BLOCK, ZONAL, LIST-DIRECTED, UNFORMATTED,
! OR ANY USER-SPECIFIED FORMAT.
! A     	Real Array to be Read from File.
! ANAME         Array Name. Only Used for Identification Purposes.
! II            Number of Rows in Array.
! JJ            Number of Columns in Array.
! K             Layer Number. Only Used for Identification Purposes.
! infUnit       Input File Unit Number.
! infName       Input File Name.
! ********************************************************
! Modified from U2DREL in MCDONALD AND HARBAUGH (1988) as Modified in Zheng (1992).
! Written By: Alaa Aly
! last modified: 11-29-01
      IMPLICIT  NONE
      INTEGER   Read_Real_Array
      INTEGER   I,J,N,II,JJ,K,infUnit,IREAD,IPRN,NBLOCK,NZONES,I1,I2,J1,J2,NN,IERR
      REAL      A,ZV,CONST,ZZ
      LOGICAL   OPD
      CHARACTER ANAME*24,FMTIN*20,infName*50
      DIMENSION A(JJ,II),ZV(:)
      ALLOCATABLE :: ZV

      DLL_EXPORT Read_Real_Array

      Read_Real_Array = 0

! Check if input file is open
      IF(infUnit .eq. 0) GOTO 468
      INQUIRE(UNIT=IABS(infUnit), OPENED = OPD)
      IF(.NOT. OPD) THEN
         IF(infUnit .gt. 0) OPEN(UNIT=IABS(infUnit),FILE=infName,STATUS='OLD',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
         IF(infUnit .lt. 0) OPEN(UNIT=IABS(infUnit),FILE=infName,STATUS='OLD',ERR=469,ACCESS='TRANSPARENT',ACTION='READ')
      ENDIF
!
!--READ ARRAY CONTROL RECORD
!  =========================
      READ (infUnit,1) IREAD,CONST,FMTIN,IPRN
    1 FORMAT(I10,F10.0,A20,I10)
!
!--IF IREAD=0, SET ALL ARRAY VALUES EQUAL TO CONST.
!  ================================================
      IF(IREAD.NE.0) GOTO 50
!
      DO 10 I=1,II
        DO 12 J=1,JJ
          A(J,I)=CONST
   12   CONTINUE
   10 CONTINUE
      GOTO 500
!
!--IF IREAD=100, INPUT ARRAY USING FORMAT FMTIN
!  ============================================
   50 IF(IREAD.NE.100) GOTO 90
!
      DO 30 I=1,II
        READ (infUnit,FMTIN) (A(J,I),J=1,JJ)
   30 CONTINUE
      GOTO 300
!
!--IF IREAD=101, INPUT ARRAY USING BLOCK FORMAT
!  ============================================
   90 IF(IREAD.NE.101) GOTO 100
!
!--READ NUMBER OF BLOCKS
      READ(infUnit,*) NBLOCK
!
!--READ VALUE OF EACH BLOCK
!--AND ASSIGN VALUE TO CELLS WITHIN THE BLOCK
      DO 70 N=1,NBLOCK
        READ(infUnit,*) I1,I2,J1,J2,ZZ
        DO 72 I=I1,I2
          DO 74 J=J1,J2
            A(J,I)=ZZ
   74     CONTINUE
   72   CONTINUE
   70 CONTINUE
      GOTO 300
!
!--IF IREAD=102, INPUT ARRAY USING ZONAL FORMAT
!  ============================================
  100 IF(IREAD.NE.102) GOTO 200
!
!--READ NUMBER OF ZONES
      READ(infUnit,*) NZONES
! alaa - canceled this in favor of dynamic memory allocation
!      IF(NZONES.GT.NZMAX) THEN
!      STOP
!      ENDIF
      ALLOCATE (ZV(NZONES),STAT=IERR)
      IF(IERR.NE.0) goto 470 ! if memory is not allocated: report an error and quit
! alaa - end of change

!--READ ZONAL MAP WITH FORMAT FMTIN
      READ(infUnit,*) (ZV(N),N=1,NZONES)
      DO 175 I=1,II
        READ(infUnit,FMTIN) (A(J,I),J=1,JJ)
  175 CONTINUE
!
!--ASSIGN ZONAL VALUES
      DO 176 I=1,II
        DO 177 J=1,JJ
          NN=A(J,I)
          IF(NN.EQ.0) THEN
            A(J,I)=0
          ELSE
            A(J,I)=ZV(NN)
          ENDIF
  177   CONTINUE
  176 CONTINUE

      DEALLOCATE(ZV)
      GOTO 300
!
!--IF IREAD=103, INPUT ARRAY USING FREE FORMAT
!  ===========================================
  200 IF(IREAD.NE.103) GOTO 250
!
!--READ ARRAY VALUES WITH FREE FORMAT
      READ(infUnit,*) ((A(J,I),J=1,JJ),I=1,II)
      GOTO 300
!
!--IF IREAD IS EQUAL TO ANY OTHER VALUES,
!--READ ARRAY VALUES FROM AN EXTERNAL FILE ON UNIT [IREAD]
!  =======================================================
!
  250 INQUIRE(UNIT=IABS(IREAD), OPENED = OPD)
      IF(.NOT. OPD) THEN
         IF(IREAD .gt. 0) then
             OPEN(UNIT=IABS(IREAD),FILE=infName,STATUS='OLD',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
             DO I=1,II
                READ (IREAD,FMTIN) (A(J,I),J=1,JJ)
             END DO
         ENDIF
         IF(IREAD .lt. 0) then
             OPEN(UNIT=IABS(IREAD),FILE=infName,STATUS='OLD',ERR=469,ACCESS='TRANSPARENT',ACTION='READ')
             READ(IABS(IREAD))
             READ(IABS(IREAD)) A
         ENDIF
      ENDIF
      CLOSE(IABS(IREAD))

!--IF CONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY CONST.
  300 IF(CONST.EQ.0) GO TO 487
      DO 302 I=1,II
      DO 304 J=1,JJ
        A(J,I)=A(J,I)*CONST
  304 CONTINUE
  302 CONTINUE

      GOTO 487
  468 Read_Real_Array = -11
      RETURN
  469 Read_Real_Array = -10
      RETURN
  470 Read_Real_Array = -1
      RETURN
  487 continue
      Read_Real_Array = 1
  500 RETURN
      END
!_________________________________________________________________________________
function Write_Integer_Array(iArray,TEXT,NCOL,NROW,ILAY,outfUnit,outfType,outfName,IHEAD,headerUnit)
! ************************************************************
! Writes A Real 1 or 2D Integer Array To a File.
! [MODIFIED FROM MCDONALD AND HARBAUGH (1988)].
! rArray        Integer 2-D Array to be written to the output file.
! TEXT          Identifying Text. Used only to Enhance Output Documentation.
! NCOL          Number of Columns in rArray
! NROW          Number of Rows in rArray
! ILAY          Layer Number for rArray. Used only to Enhance Output Documentation.
! outfUnit      Output File Unit Number.
! outfType      Flag to Determine whether Output File is Replaced (outfType = 0) or Appended to (outfType = 1)
! outfName      Output File Name
! IHEAD         Flag to Determine whether a MODFLOW-type header is Written before the Array (IHEAD = 1) or not (IHEAD = 0)
! headerUnit    Unit Number to Appear in Array Header
! ************************************************************
! Written By: Alaa Aly
! last modified: 11-29-01
      IMPLICIT  NONE
      INTEGER   Write_Integer_Array
      INTEGER   iArray,NCOL,NROW,ILAY,outfUnit,outfType,J,I,CONST,IPRN,IHEAD,headerUnit
      CHARACTER TEXT*24,FMTOUT*20,outfName*50
      LOGICAL   OPD
      DIMENSION iArray(NCOL,NROW)

      DLL_EXPORT Write_Integer_Array

      Write_Integer_Array = 0

! Check if output file is open
      IF(outfUnit .eq. 0) GOTO 468
      INQUIRE(UNIT=IABS(outfUnit), OPENED = OPD)
      IF(.NOT. OPD) THEN
         IF(outfUnit .gt. 0 .and. outfType .eq. 0) then
             OPEN(UNIT=outfUnit,FILE=outfName,STATUS='APPEND',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE')
         else IF(outfUnit .gt. 0 .and. outfType .eq. 1) then
             OPEN(UNIT=outfUnit,FILE=outfName,STATUS='REPLACE',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE')
         else IF(outfUnit .lt. 0 .and. outfType .eq. 0) then
             OPEN(UNIT=-outfUnit,FILE=outfName,STATUS='APPEND',ERR=469,ACCESS='TRANSPARENT',ACTION='WRITE')
         else IF(outfUnit .lt. 0 .and. outfType .eq. 1) then
             OPEN(UNIT=-outfUnit,FILE=outfName,STATUS='REPLACE',ERR=469,ACCESS='TRANSPARENT',ACTION='WRITE')
         ENDIF
      ENDIF

! Prepare Output File Unit Number for Usage
      IF(headerUnit .GT. 0 .AND. outfUnit .LT. 0) headerUnit = -headerUnit
      outfUnit = IABS(outfUnit)


      CONST = 1
      IPRN  = -1
      FMTOUT(1:1) = '('

!--Write the header
      if(IHEAD .eq. 1) WRITE (outfUnit,1) headerUnit,CONST,'(',NCOL,'I5)',IPRN,TEXT,ILAY
    1 FORMAT(I10,I10,A1,I6,A3,I12,5X,A24,' - IN LAYER',I3)

!--Write array values
      do I=1,NROW
         WRITE(outfUnit,'(10000I5)') (iArray(J,I),J=1,NCOL)
      end do

      CLOSE(outfUnit)
      GOTO 487
  468 Write_Integer_Array = -11
      RETURN
  469 Write_Integer_Array = -10
      RETURN
  470 Write_Integer_Array = -1
      RETURN
  487 continue
      Write_Integer_Array = 1
  500 RETURN
      END
!
!________________________________________________________________________________________
function Write_Real_Array(rArray,TEXT,NCOL,NROW,ILAY,outfUnit,outfType,outfName,IHEAD,headerUnit)
! ****************************************************************
! Writes A Real 1 or 2D Real Array To a File.
! [MODIFIED FROM MCDONALD AND HARBAUGH (1988)].
! rArray        Real 2-D Array to be written to the output file.
! TEXT          Identifying Text. Used only to Enhance Output Documentation.
! NCOL          Number of Columns in rArray
! NROW          Number of Rows in rArray
! ILAY          Layer Number for rArray. Used only to Enhance Output Documentation.
! outfUnit      Output File Unit Number.
! outfType      Flag to Determine whether Output File is Replaced (outfType = 0) or Appended to (outfType = 1)
! outfName      Output File Name
! IHEAD         Flag to Determine whether a MODFLOW-type header is Written before the Array (IHEAD = 1) or not (IHEAD = 0)
! headerUnit    Unit Number to Appear in Array Header
! ****************************************************************
! Written By: Alaa Aly
! last modified: 11-29-01
      IMPLICIT  NONE
      INTEGER   Write_Real_Array
      INTEGER   NCOL,NROW,ILAY,outfUnit,outfType,J,I,IPRN,IHEAD,headerUnit
      REAL      CONST,rArray
      CHARACTER TEXT*24,outfName*50
      LOGICAL   OPD
      DIMENSION rArray(NCOL,NROW)

      DLL_EXPORT Write_Real_Array

      Write_Real_Array = 0

! Check if output file is open
      IF(outfUnit .eq. 0) GOTO 468
      INQUIRE(UNIT=IABS(outfUnit), OPENED = OPD)
      IF(.NOT. OPD) THEN
         IF(outfUnit .gt. 0 .and. outfType .eq. 0) then
             OPEN(UNIT=outfUnit,FILE=outfName,STATUS='APPEND',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE')
         else IF(outfUnit .gt. 0 .and. outfType .eq. 1) then
             OPEN(UNIT=outfUnit,FILE=outfName,STATUS='REPLACE',ERR=469,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE')
         else IF(outfUnit .lt. 0 .and. outfType .eq. 0) then
             OPEN(UNIT=-outfUnit,FILE=outfName,STATUS='APPEND',ERR=469,ACCESS='TRANSPARENT',ACTION='WRITE')
         else IF(outfUnit .lt. 0 .and. outfType .eq. 1) then
             OPEN(UNIT=-outfUnit,FILE=outfName,STATUS='REPLACE',ERR=469,ACCESS='TRANSPARENT',ACTION='WRITE')
         ENDIF
      ENDIF

! Prepare Output File Unit Number for Usage
      IF(headerUnit .GT. 0 .AND. outfUnit .LT. 0) headerUnit = -headerUnit
      outfUnit = IABS(outfUnit)

      CONST = 1.0
      IPRN  = -1

!--Write the header
      if(IHEAD .eq. 1) WRITE (outfUnit,1) headerUnit,CONST,'(',NCOL,'I5)',IPRN,TEXT,ILAY
    1 FORMAT(I10,F10.3,A1,I6,A3,I12,5X,A24,' - IN LAYER',I3)

!--Write array values
      do I=1,NROW
         WRITE(outfUnit,'(10000G30.7)') (rArray(J,I),J=1,NCOL)
      end do

      CLOSE(outfUnit)
      GOTO 487
  468 Write_Real_Array = -11
      RETURN
  469 Write_Real_Array = -10
      RETURN
  470 Write_Real_Array = -1
      RETURN
  487 continue
      Write_Real_Array = 1
  500 RETURN
      END

function Write_Binout(Binname,Nval,Values,NameLen,NameString,Opname,OpID,Section,UnitFlag,PrintLevel,CurDate,First)
! ****************************************************************
! Writes a real array of values as a record of an HSPF Binary Output file
! Binname      Name of binary output file
! Nval          Number of values
! Values        Real 1-D Array to be written to the output file.
! NameLen       Lengths of variable names (Max length of ten each)
! NameString    String of variable names, starting at positions 1, 11, 21,...
! Opname        Operation type name (PERLND, IMPLND, RCHRES, etc)
! OpID          Operation ID number
! Section       Section name (PWATER, etc)
! UnitFlag      1=English, 2=Metric - simply passed on to file
! PrintLevel    3=Daily, 4=Monthly, 5=Annually (2=Interval not implemented?)
! CurDate       Year, Month, Day, Hour, Minute, Second (Seconds not written to file)
! First         Logical: True if first time through
! 
! ****************************************************************
! Written By: Tom Jobes, AQUA TERRA Consultants
! last modified: 11-Mar-2002
      IMPLICIT  NONE
      INTEGER   Write_Binout

      INTEGER       Nval,NameLen(Nval),OpID,UnitFlag,PrintLevel,CurDate(6)
      REAL          Values(Nval)
      CHARACTER*255 Binname
      CHARACTER*255 NameString
      CHARACTER*6   Opname,Section
      CHARACTER*10  Varnames(300)
      LOGICAL       First

      INTEGER       I,J,I0,I1,BINU,IOS,ERRFLG,x,k1,k2
      CHARACTER*2   Space

      DATA I0,I1,BINU,x/0,1,99,98/
      DATA Space/'  '/

      DLL_EXPORT Write_Binout

      ERRFLG = 0
      IF (First) THEN
        !at start of run, write the header
        OPEN (BINU,FILE=Binname,ACCESS='SEQUENTIAL',FORM='UNFORMATTED', STATUS='UNKNOWN',IOSTAT=IOS,ERR=900)
        OPEN (x,FILE='test.txt',ACCESS='SEQUENTIAL',FORM='FORMATTED', STATUS='UNKNOWN',IOSTAT=IOS,ERR=900)

        do 10 i= 1, nval
          do 5 j=1, namelen(nval)
           k1= 10*(i-1)+1
           k2= 10*(i-1)+NameLen(i)
           WRITE (X,*) namestring(k1:k2)
  5       continue
 10     continue

        WRITE (BINU) I0,Opname,Space,OpID,Section,Space, (NameLen(i),NameString(10*(i-1)+1:10*(i-1)+NameLen(i)),i=1,Nval)
        WRITE (x,*) I0,Opname,Space,OpID,Section,Space, (NameLen(i),NameString(10*(i-1)+1:10*(i-1)+NameLen(i)),i=1,Nval)
      ELSE
        !append 
        OPEN (BINU,FILE=Binname,ACCESS='SEQUENTIAL',FORM='UNFORMATTED', STATUS='OLD',POSITION='APPEND',ERR=910)

      END IF
      WRITE (BINU) I1,Opname,Space,OpID,Section,Space,UnitFlag,PrintLevel, (CurDate(I),I=1,5),(Values(I),I=1,Nval)

      CLOSE (BINU)
      GO TO 999

 900  CONTINUE

        ERRFLG = 1
        WRITE (*,*) 'ERROR OPENING FILE',Binname, 'FOR WRITE',IOS-16384
        GO TO 999

 910  CONTINUE
        ERRFLG = 2
        WRITE (*,*) 'ERROR OPENING FILE',Binname, 'FOR APPEND',IOS-16384
        GO TO 999

 999  CONTINUE

      Write_Binout = ERRFLG

      RETURN
      END

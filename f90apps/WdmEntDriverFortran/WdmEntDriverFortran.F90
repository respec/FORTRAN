      Program WdmEntDriverFortran
      
!DEC$ ATTRIBUTES DLLIMPORT:: F90_WDBOPN, F90_WDCKDT

!     local variables
      Integer*4    :: WDMSFL, RETCOD, DSN, DSTYPE, I
      Character*64 :: WDNAME, WDMNAMES(3)

!     functions
      Logical*4    :: Find_Test_Folder
      Integer*4    :: F90_WDMOPN, F90_WDBOPN
      Integer*4    :: F90_WDMCLO, F90_WDFLCL
      Integer*4    :: F90_WDCKDT

      Write(*,*) 'Start WdmEntDriverFortran'
      
      WDMNAMES(1) = 'test.wdm'
      WDMNAMES(2) = 'missing.wdm'
      WDMNAMES(3) = 'corrupt.wdm'
  
      IF (Find_Test_Folder() .EQ. .TRUE.) THEN
        DO I = 1, 3
          WDNAME = WDMNAMES(I)
          WDMSFL = 40
          RETCOD = F90_WDMOPN(WDMSFL,WDNAME)
          Write(*,*) 'F90_WDMOPN Return Code ', RETCOD, ' Opening ', Trim(WDNAME)
          RETCOD = F90_WDMCLO(WDMSFL)
          Write(*,*) 'F90_WDMCLO Return Code ', RETCOD, ' Closing ',WDMSFL
        
          WDMSFL = F90_WDBOPN(0,WDNAME)
          IF (WDMSFL .LT. 0) THEN
            Write(*,*) 'F90_WDBOPN Return Code ', RETCOD, ' Opening ', Trim(WDNAME)
          ELSE
            Write(*,*) 'F90_WDBOPN Return Code ', WDMSFL, ' Opening ', Trim(WDNAME)
            DSN = 39
            DSTYPE = F90_WDCKDT(WDMSFL, DSN)
            Write(*,*) 'F90_WDCKDT: DSN, TYPE: ', DSN, DSTYPE
          END IF
          RETCOD = F90_WDFLCL(WDMSFL)
          Write(*,*) 'F90_WDFLCL Return Code ', RETCOD
        END DO
      ELSE
        Write(*,*) 'Test Folder Not Found'
      END IF
      
      Write(*,*) 'End WdmEntDriverFortran'
      
      End Program
      
      Logical Function Find_Test_Folder 
      
      USE IFPORT

      Integer*4           :: LEN
      Logical             :: RET
      Character($MAXPATH) :: DIR

      Find_Test_Folder = .FALSE.
      
      !  Get current directory
      LEN = GETDRIVEDIRQQ(DIR)
      IF (LEN .GT. 0) THEN  
          WRITE (*,*) 'Base directory is: ',DIR(1:LEN)
          DIR = DIR(1:LEN) // '\test'
          RET = CHANGEDIRQQ(DIR) 
          LEN = GETDRIVEDIRQQ(DIR)
          IF (LEN .GT. 0) THEN
            WRITE (*,*) 'Test directory is: ',DIR(1:LEN)        
            Find_Test_Folder = .TRUE.
          ELSE
            WRITE (*,*) 'Failed to set test directory'
          END IF
      ELSE  
          WRITE (*,*) 'Failed to get current directory'
      END IF      
      
      End Function
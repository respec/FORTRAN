      Program WdmEntDriverFortran
      
!DEC$ ATTRIBUTES DLLIMPORT:: F90_WDBOPN, F90_WDCKDT

      Logical*4    :: Find_Test_Folder
      Integer*4    :: WDMSFL, DSN, DSTYPE, F90_WDBOPN, F90_WDCKDT
      Character*64 :: WDNAME

      Write(*,*) 'Start WdmEntDriverFortran'
  
      IF (Find_Test_Folder() .EQ. .TRUE.) THEN
        WDNAME = 'test.wdm'
        WDMSFL = F90_WDBOPN(WDNAME)
        IF (WDMSFL .LT. 0) THEN
          Write(*,*) 'Return Code ', RETCOD, ' Opening ', WDNAME
        ELSE
          DSN = 39
          DSTYPE = F90_WDCKDT(WDMSFL, DSN)
          Write(*,*) '  DSN, TYPE: ', DSN, DSTYPE
        END IF
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
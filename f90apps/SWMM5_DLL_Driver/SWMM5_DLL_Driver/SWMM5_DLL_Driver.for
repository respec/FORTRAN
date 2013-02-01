       Program SWMM5_DLL_Driver

       Use IFPORT

       Implicit None


       Interface
           Integer Function swmm_getVersion() 
     1                     Bind(C,Name='swmm_getVersion@0')
           End Function swmm_getVersion
           Integer Function swmm_open(C1,C2,C3)
     1                     Bind(C,Name='swmm_open@12')
              Character C1,C2(*),C3(*)
           End Function swmm_open
       End Interface

       Integer       i
       Logical       status
       Character*128 CurDir,BaseFileName,
     1               InputFileName,OutputFileName,BinaryFileName
       
       Write(*,*) 'Entry SWMM5_DLL_Driver'
             
       status= CHANGEDIRQQ('..\Data')
       CurDir = "C:"
       i = GETDRIVEDIRQQ(CurDir)
       Write(*,*) 'Current directory "' // CurDir(1:i) // '"'

       i= swmm_getVersion()
       Write(*,*) 'SWMM Version ', i
       
       BaseFileName = 'FandM_Base'
       InputFileName= trim(BaseFileName) // '.inp' // Char(0)
       OutputFileName = trim(BaseFileName) // '.out' // Char(0)
       BinaryFileName = trim(BaseFileName) // '.bin' // Char(0)

       Write(*,*) 'SWMM Open Input "' // Trim(InputFileName) // '"'
       i= swmm_open(InputFileName, OutputFileName, BinaryFileName)
       Write(*,*) 'SWMM Open Done, Return Code ',i
       
       Stop
       End
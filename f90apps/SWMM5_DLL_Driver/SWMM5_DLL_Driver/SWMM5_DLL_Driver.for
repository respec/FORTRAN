       Program SWMM5_DLL_Driver

       Implicit None

       Interface
           Integer Function swmm_getVersion() 
     1                     Bind(C,Name='swmm_getVersion@0')
           End Function swmm_getVersion
       End Interface

       Integer i 
       
       Write(*,*) "Entry SWMM5_DLL_Driver"
       
       i= swmm_getVersion()
       Write(*,*) "SWMM Version ", i
       
       Stop
       End
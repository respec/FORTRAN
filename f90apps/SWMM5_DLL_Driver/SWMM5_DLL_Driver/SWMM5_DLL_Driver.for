       Program SWMM5_DLL_Driver

!DEC$ ATTRIBUTES DECORATE,ALIAS:'swmm_getVersion' :: swmm_getVersion
        
       Integer swmm_getVersion
       External swmm_getVersion
       
       Write(*,*) "Entry SWMM5_DLL_Driver"
       
       i= swmm_getVersion()
       Write(*,*) "SWMM Version ", i
       
       Stop
       End
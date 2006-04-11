C
C     VADOFT storage variables
C
C-----Next 2 arrays for storing VADOFT values between VADOFT runs
      REAL SAVHED(MXNSZO,MXNOD)                                         
      REAL SAVCNC(MXNSZO,MXNOD,3)                                       
C-----Array for storing VADOFT corrected values of head and conc.
      REAL PINT(MXNOD)                                                  
C-----VARS. TO PASS FLUX TO VADOFT
      REAL TOPFLX(MXVDT), TOWFLX(MXVDT)                                 
C
      COMMON /VADSTR41/ PINT, TOPFLX, TOWFLX
C 
      COMMON /VADSTR42/ SAVHED 
C
      COMMON /VADSTR43/  SAVCNC
C

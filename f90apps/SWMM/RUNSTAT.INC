C==========================================================================
C NEW COMMON FOR MAXIMUM VALUES IN CHANNELS/PIPES
      COMMON/MAX/MAXJUL(NG),MAXTIM(NG),MAXDEP(NG),
     1           SURLEN(NG),MAXSUR(NG),MAXQIN(NG)
      REAL MAXDEP,MAXSUR,MAXQIN,MAXTIM
C==========================================================================
C NEW COMMON FOR SUBCATCHMENT STATISTICS
      COMMON/MAX2/SUBQPEAK(6,NW),SUBDEP(4,NW)
C========================================================================
C NEW COMMON TO GET CORRECT TOTAL RAINFALL TO PRINT IN WSTAT
CIM INCREASE HYETOGRAPHS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      COMMON/TOTRAIN/RTOT(MAXRG)
cim       COMMON/TOTRAIN/RTOT(10)
cim   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

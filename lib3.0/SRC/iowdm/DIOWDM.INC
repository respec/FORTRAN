C
C     time-series arrays, sizes, and pointers
C     MXVAL  - maximum number of values in VAL
C     DATES  - date array
C              (_,1) - starting date of time-series array
C              (_,2) - next date to be added to time-series array
C              (_,3) - ending date of time-series array
C     IPT    - pointer to last value in time-series array VAL
C     IPTN   - pointer for next expected position in array VAL
C     KTREC  - number of input records processed
C     KTERR  - number of invalid records processed
C     TSSTEP - time step of data being processed
C     TUNITS - time units of data being processed
C     DTOVWR - data overwrite flag
C              0 - don't overwrite data
C              1 - ok to overwrite existing data
C     QFLAG  - quality flag of data being processed
C              application specific, usually 0
C              if QFLAG = -1, quality codes are in valq
C     VAL    - array containing time series data
C     VALQ   - array containing quality code for time series val
C              not used if QFLAG >= 0

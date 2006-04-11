C
C     common block for time series list parameters
C
      INTEGER    MAXD,   LSUM
      PARAMETER (MAXD=30,LSUM=5*MAXD)
C     MAXD = maximum number of datasets for each listing
C     LSUM = 5 times MAXD for 5 sums for each dataset
C
      COMMON /CTSLST/ SDATE,   EDATE,   TUNITS,TSTEP,TORF,FUNIT,
     $                WIDTH,LINES,SGFD,DPLA,GRPMAX,TRANS,
     $                QFLG,SNUM,ENDMO,LORS,TOTAVE,THRSH
C
      INTEGER         SDATE(6),EDATE(6),TUNITS,TSTEP,TORF,FUNIT,
     $                WIDTH,LINES,SGFD,DPLA,GRPMAX,TRANS(MAXD),
     $                QFLG,SNUM,ENDMO,LORS,TOTAVE(5)
      REAL            THRSH(4)

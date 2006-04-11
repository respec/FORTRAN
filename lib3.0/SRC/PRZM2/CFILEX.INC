C
C     Include File CFILEX.INC
C
      INTEGER  FILOPC,FPRZIN(MXZONE),FPRZRS(MXZONE),
     1         FPRZOT(MXZONE),FMETEO(MXZONE),FSPTIC(MXZONE),
     2         FNITAD(MXZONE),FVADIN(MXNSZO),FVADOT(MXNSZO),
     3         FVTP10(MXNSZO),FVRSTF(MXNSZO),FVRSTT(MXNSZO),
     4         FTMSRS(MXZONE),FMCIN,FMCOUT,FMCOU2,FCON,FECHO,
     5         FWDMS
C
      COMMON /FILEX/ FILOPC,FPRZIN,FPRZRS,FWDMS,
     1               FPRZOT,FMETEO,FSPTIC,FNITAD,FVADIN,FVADOT,FVTP10,
     2               FVRSTF,FVRSTT,FTMSRS,FMCIN,FMCOUT,FMCOU2,FCON,FECHO
C
C     FILOPC - number of files open
C     FPRZIN - przm input file unit number
C     FPRZRS - przm restart file unit number
C     FPRZOT - przm output file unit number
C     FMETEO - meteorology file unit number
C     FSPTIC - septic effluent file unit number
C     FNITAD - nitrogen atmospheric deposition file unit number
C     FVADIN - vadoft input file unit number
C     FVADOT - vadoft output file unit number
C     FVTP10 - vadoft tape 10 file unit number
C     FVRSTF - vadoft flow restart file unit number
C     FVRSTT - vadoft transport restart file unit number
C     FTMSRS - time series file unit number
C     FMCIN  - ???
C     FMCOUT - ???
C     FMCOU2 - ???
C     FCON   - ???
C     FECHO  - ???
C     FWDMS  - wdms file unit number
C

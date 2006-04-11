C
C     include file PPARM.INC
C
      INTEGER    NCMPTS, NAPP, NC, NPII, NCMPP2, MXCPD
C
C      PARAMETER (NCMPTS=100, NAPP=50, NC=5, NPII=800)
      PARAMETER (NCMPTS=200, NAPP=400, NC=5, NPII=800)
C
C     NCMPTS - number of przm compartments
C     NAPP   - number of pesticides applications
C     NC     - number of crops
C     NPII   - number of points for moc algorithm.
C
C     the dimension for 'NCMPTS' must be at least one number larger
C     than the number of compartments that are being simulated.
C     note if NPII value is changed, write statements in lines 106-123
C     of subroutine RSTPT1 and read statements in lines 120-138 of
C     subroutine RSTGT1 for variable CC need to be modified accordingly.
C
      PARAMETER (NCMPP2=NCMPTS+2)                                       
C
C     maximum number of cropping periods
      PARAMETER (MXCPD=100)
C

C=======================================================================
C     NVORF  ==> Number of variable orifices.
C     NVOTIM ==> Number of data points for variable orifices.
C=======================================================================
      PARAMETER(NVORF=20,NVOTIM=25)
      COMMON/ORF/LORIF(NEO),AORIF(NEO),CORIF(NEO),
     +           VORIF(NVORF,NVOTIM,3),NVOR,NORIF

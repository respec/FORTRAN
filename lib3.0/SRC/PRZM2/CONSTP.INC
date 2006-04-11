C     These COMMON block allows for the storage of machine dependent
C     numeric constants for the PRZM2 model.
C
      INTEGER          RPREC,DPREC
      REAL             R0MIN,RP1MIN,R0MAX
      DOUBLE PRECISION D0MIN,DP1MIN,D0MAX
C
      COMMON /IPCONS/ RPREC,DPREC
      COMMON /RPCONS/ R0MIN,RP1MIN,R0MAX
      COMMON /DPCONS/ D0MIN,DP1MIN,D0MAX
C
C     RPREC  - the number of (decimal) digits of precision of a real number.
C     DPREC  - the number of (decimal) digits of precision of a double.
C     R0MIN  - the smallest (positive) representable real number.
C     RP1MIN - the minimum representable real number greater than (plus) 1.0.
C     R0MAX  - the largest representable real number.
C     D0MIN  - the smallest (positive) representable double.
C     DP1MIN - the minimum representable double greater than (plus) 1.0.
C     D0MAX  - the largest representable double.


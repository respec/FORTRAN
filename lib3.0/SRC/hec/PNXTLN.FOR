      SUBROUTINE PNXTLN (K, ISS, NL, NLX, ML, MLX, CLINX, LEND)
C
C     This routine returns the next line from the translate
C     Table or append table.
C
CADD C.PSCNCH                                                           H
      INCLUDE 'pscnch.h'                                                MLlg
C
      CHARACTER CLINX*(*)
      LOGICAL LEND
C
C
C
      IF(NL.GT.0) THEN
      IS=ISS     + (NLX - NL) * IB(13,K) - 1
      NL=NL - 1
      ELSE IF(ML.GT.0) THEN
      IS=IB(1,K) + IB(9,K) + 09 + (MLX - ML) * IB(13,K)
      ML=ML-1
      ELSE
      LEND=.TRUE.
      RETURN
      ENDIF
      DO 10 I=1,IB(13,K) - 11
   10 CLINX(I:I) = CSCN(IS+I)
      LEND=.FALSE.
      RETURN
      END

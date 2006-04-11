      SUBROUTINE PUFBFW  ( IBUFF, IROW, ICOL, NCOLS, NROWS )
C
      INTEGER*2 IBUFF(NCOLS, NROWS)
      INTEGER*4 INTARY(9)
      INTEGER*4 EAX, EBX, ECX, EDX
      INTEGER*4 DS, ES, EDI, ESI, FLG
      INTEGER*4 AH, AL
C
      EQUIVALENCE ( INTARY(1), EAX )
      EQUIVALENCE ( INTARY(2), EBX )
      EQUIVALENCE ( INTARY(3), ECX )
      EQUIVALENCE ( INTARY(4), EDX )
      EQUIVALENCE ( INTARY(5), DS  )
      EQUIVALENCE ( INTARY(6), ES  )
      EQUIVALENCE ( INTARY(7), EDI )
      EQUIVALENCE ( INTARY(8), ESI )
      EQUIVALENCE ( INTARY(9), FLG )
C
      N = 0
C
      DO 100 I=IROW, IROW+NROWS-1
C
      M = 0
      N = N+1
C
      DO 200 J=ICOL, ICOL+NCOLS-1
C
      M = M+1
C
C     CALL PUFCUR ( I, J )
C
C --- POSITION CURSOR ON SCREEN
C
C     EAX = ICAT ( 2, 0 )
C     EBX = ICAT ( 0, 0 )
C     EDX = ICAT ( I, J )
C
      EAX = 512
      EBX = 0
      EDX = I*256 + J
C
      NTRUP = 16
      CALL INTRUP ( INTARY,  NTRUP )
C
C
C     CALL DCAT ( IBUFF(M,N), AH, AL )
      AL = IAND(IBUFF(M,N),255)
      AH = IAND(ISHFT(IBUFF(M,N),-8),255)
C
C     EAX = ICAT ( 9, AL )
C     EBX = ICAT ( 0, AH )
C     ECX = 1
      EAX = 2304 + AL
      EBX = AH
      ECX = 1
      CALL INTRUP ( INTARY, 16 )
C
  200 CONTINUE
  100 CONTINUE
C
      RETURN
      END

      SUBROUTINE PUFA ( IATT, NCHS, IROW, ICOL )
C
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
      DO 100 I=ICOL, ICOL+NCHS-1
C
C     CALL PUFCUR ( IROW, I )
C
C --- POSITION CURSOR ON SCREEN
C
C     EAX = ICAT ( 2, 0 )
C     EBX = ICAT ( 0, 0 )
C     EDX = ICAT ( IROW, I )
C
      EAX = 512
      EBX = 0
      EDX = IROW*256 + I
C
      NTRUP = 16
      CALL INTRUP ( INTARY,  NTRUP )
C
C
C     EAX = ICAT ( 8, 0 )
C     EBX = ICAT ( 0, 0 )
      EAX = 2048
      EBX = 0
      NTRUP = 16
      CALL INTRUP ( INTARY, NTRUP )
C
C     CALL DCAT ( EAX, AH, AL )
      AL = IAND(EAX,255)
      AH = IAND(ISHFT(EAX,-8),255)
C
C     EAX = ICAT ( 9, AL )
C     EBX = ICAT ( 0, IATT )
C     ECX = 1
      EAX = 2304 + AL
      EBX = IATT
      ECX = 1
      CALL INTRUP ( INTARY, NTRUP )
C
  100 CONTINUE
C
      RETURN
      END

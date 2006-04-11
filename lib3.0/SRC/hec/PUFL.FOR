      SUBROUTINE PUFL ( CLINE, NCHS, IROW, ICOL )
C
      CHARACTER CLINE*(*)
      INTEGER*4 INTARY(9)
      INTEGER*4 EAX, EBX, ECX, EDX
      INTEGER*4 DS, ES, EDI, ESI, FLG
      INTEGER*4 AL
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
      J = 0
      DO 100 I = ICOL, ICOL+NCHS-1
      J = J + 1
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
      AL = ICHAR ( CLINE(J:J) )
C     EAX = ICAT ( 10, AL )
C     EBX = ICAT ( 0, 0 )
C     ECX = ICAT ( 0, 1 )
      EAX = 2560 + AL
      EBX = 0
      ECX = 1
      CALL INTRUP ( INTARY, 16 )
C
  100 CONTINUE
C
      RETURN
      END

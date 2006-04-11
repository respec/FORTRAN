      SUBROUTINE ERASF ( CNAME, ISTAT )
C
C     Erases a file using DOS interrupt 21h / function 13h
C
C     CNAME  - Name of file to be erased ( terminated with CHAR(0) )
C
C     ISTAT  - Status of DOS function call
C              0 = call to ERASF was successful
C              Nonzero = error code found on page 6-42 of DOS Technical
C                        Reference Manual
C
      CHARACTER CNAME*(*)
      INTEGER*4 ISTAT
C
      INTEGER*4 INTARY(9)
      INTEGER*4 EAX, EBX, ECX, EDX
      INTEGER*4 DS, ES, EDI, ESI, FLG
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
C     Specify interrupt 21h ( 21h ===> 33d )
C
      NTRUP = 33
C
C     Specify function 13h ( 1300h ===> 4864d ) in the AH register
C
      EAX = 4864
      EDX = OFFSET( CNAME )
C
      CALL INTRUP ( INTARY, NTRUP )
C
C     If carry flag is set, return the extended error status in ISTAT.
C     If carry flag is not set, return the file handle in IHANDL.
C
      IF ( IAND( EAX, 65520 ) .NE. 0 ) THEN
         ISTAT  = EAX
      ELSE
         ISTAT  = 0
      ENDIF
C
      RETURN
      END

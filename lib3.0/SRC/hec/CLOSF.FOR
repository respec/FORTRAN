      SUBROUTINE CLOSF ( IHANDL, ISTAT )
C
C     Closes a file using DOS interrupt 21h / function 3Eh
C
C     IHANDL - File handle of file to be closed. Determined from call
C              to OPENF
C
C     ISTAT  - Status of DOS function call
C              0 = call to CLOSF was successful
C              Nonzero = error code found on page 6-42 of DOS Technical
C                        Reference Manual
C
      INTEGER*4 IHANDL, ISTAT
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
C     Specify function 3Eh ( 3E00h ===> 15872d ) in the AH register
C
      EAX = 15872
      EBX = IHANDL
C
      CALL INTRUP ( INTARY, NTRUP )
C
C     If carry flag is set, return the extended error status in ISTAT.
C     If carry flag is not set, return ISTAT=0.
C
      IF ( IAND( FLG, 1 ) .NE. 0 ) THEN
         ISTAT  = EAX
      ELSE
         ISTAT  = 0
      ENDIF
C
      RETURN
      END

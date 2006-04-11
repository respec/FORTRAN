      SUBROUTINE WRITF ( IHANDL, IBUFF, NBYTES, ISTAT, NTRANS )
C
C     Writes to a file using DOS interrupt 21h / function 40h
C
C     IHANDL - File handle returned by successful call to OPENF
C
C     IBUFF  - Integer buffer containing the information to write
C
C     NBYTES - The number of bytes to be written to the file
C
C     ISTAT  - Status of DOS function call
C              0 = call to WRITF was successful
C              Nonzero = error code found on page 6-42 of DOS Technical
C                        Reference Manual
C
C     NTRANS - The number of bytes that were actually written
C
      INTEGER*4 IHANDL, IBUFF, NBYTES, ISTAT, NTRANS
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
C     Specify function 40h ( 4000h ===> 16384d ) in the AH register
C
      EAX = 16384
      EBX = IHANDL
      ECX = NBYTES
      EDX = OFFSET( IBUFF )
C
      CALL INTRUP ( INTARY, NTRUP )
C
C     If carry flag is set, return the extended error status in ISTAT.
C     If carry flag is not set, return number of bytes read in NTRANS.
C
      IF ( IAND( FLG, 1 ) .NE. 0 ) THEN
         ISTAT  = EAX
         NTRANS = 0
      ELSE
         ISTAT  = 0
         NTRANS = EAX
      ENDIF
C
      RETURN
      END

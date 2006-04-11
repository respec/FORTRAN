      SUBROUTINE SEEKF ( IHANDL, IMODE, IOFSET, IPOS, ISTAT )
C
C     Repositions pointer in file using DOS interrupt 21h / function 42h
C
C     IHANDL - File handle returned by successful call to OPENF
C
C     IMODE  - Mode of the offset
C              0 = The offset is from the beginning of the file
C              1 = The offset is from the current location
C              2 = The offset is from the end of the file
C
C     IOFSET - The number of bytes to move
C
C     IPOS   - The resulting file byte position after the move
C
C     ISTAT  - Status of DOS function call
C              0 = call to SEEKF was successful
C              Nonzero = error code found on page 6-42 of DOS Technical
C                        Reference Manual
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
C     Specify function 42h ( 4200h ===> 16896d ) in the AH register
C
      EAX = 16896 + IMODE
      EBX = IHANDL
      EDX = IOFSET
C
      CALL INTRUP ( INTARY, NTRUP )
C
C     If carry flag is set, return the extended error status in ISTAT.
C     If carry flag is not set, return number of bytes read in NTRANS.
C
      IF ( IAND( FLG, 1 ) .NE. 0 ) THEN
         ISTAT  = EAX
         IPOS   = 0
      ELSE
         ISTAT  = 0
         IPOS   = EAX
      ENDIF
C
      RETURN
      END

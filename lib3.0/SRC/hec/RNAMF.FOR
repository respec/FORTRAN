      SUBROUTINE RNAMF ( COLDN, CNEWN, ISTAT )
C
C     Renames a file using DOS interrupt 21h / function 17h
C
C     COLDN  - Name of file to be renamed ( terminated with CHAR(0) )
C
C     CNEWN  - New name of file ( terminated with CHAR(0) )
C
C     ISTAT  - Status of DOS function call
C              0 = call to RNAMF was successful
C              Nonzero = error code found on page 6-42 of DOS Technical
C                        Reference Manual
C
      CHARACTER COLDN*(*), CNEWN*(*)
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
C     Specify function 17h ( 1700h ===> 5888d ) in the AH register
C
      EAX = 5888
      EDX = OFFSET( CNEWN )
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

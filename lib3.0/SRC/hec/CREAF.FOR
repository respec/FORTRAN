      SUBROUTINE CREAF ( CNAME, IFATT, IHANDL, ISTAT )
C
C     Opens a file using DOS interrupt 21h / function 3Ch
C
C     CNAME  - Name of file to be opened ( terminated with CHAR(0) )
C
C     IFATT  - Attributes of the file to be opened
C              0 = normal file used for reading and writing
C              1 = file opened for read only
C              2 = file opened as a hidden file
C              4 = file opened as a system file
C              8 = file opened is a volume label and exists in the root
C             16 = file opened is a subdirectory
C             32 = file opened with archive bit set
C
C     IHANDL - File handle returned by the subroutine with successful open
C
C     ISTAT  - Status of DOS function call
C              0 = call to CREAF was successful
C              Nonzero = error code found on page 6-42 of DOS Technical
C                        Reference Manual
C
      CHARACTER CNAME*(*)
      INTEGER*4 IFATT, IHANDL, ISTAT
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
C     Specify function 3Ch ( 3C00h ===> 15360d ) in the AH register
C
      EAX = 15360
      ECX = IFATT
      EDX = OFFSET( CNAME )
C
      CALL INTRUP ( INTARY, NTRUP )
C
C     If carry flag is set, return the extended error status in ISTAT.
C     If carry flag is not set, return the file handle in IHANDL.
C
      IF ( IAND( FLG, 1 ) .NE. 0 ) THEN
         ISTAT  = EAX
         IHANDL = 0
      ELSE
         ISTAT  = 0
         IHANDL = EAX
      ENDIF
C
      RETURN
      END

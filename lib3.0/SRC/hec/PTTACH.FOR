       SUBROUTINE PTTACH (IUNIT,CKEY,CDEF,CONTRL,CSTR,ISTAT)
C
C     --- This routine allows the the PREAD utility to use the
C         command line parameters without opening the actual
C         files associated with them.
C
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
C
      CHARACTER CKEY*(*), CDEF*(*), CONTRL*(*),CSTR*(*)
      CHARACTER *6 CTRL
C
      ENTRY PATTCH (IUNIT,CKEY,CDEF,CONTRL,CSTR,ISTAT)
      LPATT = .TRUE.
      CTRL = 'NOP'
      IF (CKEY(1:3) .EQ. 'SCR') THEN
         ISCT = IUNIT
         CTRL = ' '
      ELSE IF (CKEY(1:3) .EQ. 'FUN') THEN
         IFUN = IUNIT
      ELSE IF (CKEY(1:3) .EQ. 'MAC') THEN
         IMAC = IUNIT
      ELSE IF (CKEY(1:3) .EQ. 'MEN') THEN
         IMEN = IUNIT
      ELSE IF (CKEY(1:3) .EQ. 'LOG') THEN
         ILOG = IUNIT
      ELSE IF (CKEY(1:3) .EQ. 'SCN') THEN
         ISCN = IUNIT
      ELSE
         CTRL = CONTRL
      ENDIF
      CALL ATTACH (IUNIT,CKEY,CDEF,CTRL,CSTR,ISTAT)
      IF (CKEY(1:3) .EQ. 'SCR') THEN
         CSCTFL = CSTR
      ELSE IF (CKEY(1:3) .EQ. 'FUN') THEN
         CFUNFL = CSTR
      ELSE IF (CKEY(1:3) .EQ. 'MAC') THEN
         CMACFL = CSTR
      ELSE IF (CKEY(1:3) .EQ. 'MEN') THEN
         CMENFL = CSTR
      ELSE IF (CKEY(1:3) .EQ. 'LOG') THEN
         CLOGFL = CSTR
      ELSE IF (CKEY(1:3) .EQ. 'SCN') THEN
         CSCNFL = CSTR
      ENDIF
      RETURN
      END

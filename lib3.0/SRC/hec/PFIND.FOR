      SUBROUTINE PFIND (LFN, STRING, LINE1, UP, FOUND, IDSP)
C
C     Searches file, attached to LFN, for STRING.
C     Begin search at LINE1.
C
C     Input Parameters -------------------------------------------------
C        LFN    - Logical file number
C        STRING - String to look for in file attached to LFN
C        LINE1  - Line number where search starts
C        UP     - Direction to search in file from LINE1,
C                 up is TRUE, down is FALSE
C        IDSP   - Unit number for console
C
C     Output Parameters ------------------------------------------------
C        LINE1 - Line number where file is positioned
C        FOUND - TRUE is STRING was found, otherwise FALSE
C
      CHARACTER STRING*(*)
      INTEGER LFN, LINE1
      LOGICAL UP, FOUND
C
      CHARACTER LINE*133
C
C ======================================================================
C
C     --- Search for STRING
      FOUND = .FALSE.
      CALL POSFL (LFN, LINE1, ISTAT)                                    MLlg
C     CALL GIOPLW (LFN,'23,IDUM,LINE1-1,ISTAT)                          H
      L = LINE1
 1000 READ (LFN,'(A)',END=1200) LINE
      IF ( INDEX (LINE, STRING) .GT. 0 ) THEN
         FOUND = .TRUE.
      ENDIF
C
      IF (.NOT. FOUND) THEN
         IF ( .NOT. UP ) THEN
            L = L + 1
            GO TO 1000
         ELSE IF ( L .GT. 1 ) THEN
            BACKSPACE LFN
            BACKSPACE LFN
            L = L - 1
            GO TO 1000
         ELSE
            WRITE (IDSP,*) '< TOP OF FILE >'
         ENDIF
      ENDIF
      GO TO 1300
C
 1200 CONTINUE
      WRITE (IDSP,*) '< END OF FILE >'
      L = L - 1
C
 1300 CONTINUE
      LINE1 = L
      RETURN
      END

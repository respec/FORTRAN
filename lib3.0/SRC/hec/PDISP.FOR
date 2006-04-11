      SUBROUTINE PDISP (OP, COPT, UP)
C
C     Performs DISPLAY, LIST, and FIND commands.
C
C     Input parameters -------------------------------------------------
C        OP - operation 'DISP' or 'FIND'
C        COPT - options
C        UP - direction of display or search relative to current
C              position; TRUE is upward, FALSE is downward.
C
C     Options ----------------------------------------------------------
C        .T - Text only
C        .F - Frame & text (default)
C
C
      CHARACTER OP*(*)
      LOGICAL UP
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
C
C     Local Variables --------------------------------------------------
C        FILNAM - default file name
C        LINE1  - first line
C        LINE2  - last line
C        COL1   - first column
C        COL2   - last column
C        LPP    - lines per page (screen)
C        LFN    - unit number of file
C
      CHARACTER FILNAM*17, STRING*133
      CHARACTER CNAME*17, IFMT*4, LINE*133
      CHARACTER LINENO*11, COLNO*7
      INTEGER DELAY, FIELD
      INTEGER LINE1, LINE2, COL1, COL2, IFND
      LOGICAL FOUND, NEWFIL
      LOGICAL LISFIL
      CHARACTER COPT*8
      LOGICAL LFRAME,LCOPTS
      EXTERNAL LCOPTS
C
C
      DATA COL1,COL2 /1,80/
      DATA IFMT /'(I1)'/
      DATA LPP,IFND /24,0/
C
C ----------------------------------------------------------------------
C
 2000 FORMAT (/' *** FILE ',A,5X,'LINES ',A,5X,'COLUMNS ',A,' ***'/)
C
C ======================================================================
C
C     --- Get parameters from command line
C     --- Get any options stored in COPT, & set logical for frame
C
      LFRAME=.TRUE.
      IF(LCOPTS(COPT,'T')) LFRAME=.FALSE.
      IF(LCOPTS(COPT,'F')) LFRAME=.TRUE.
C
C
      FIELD = 2
      NEWFIL = .FALSE.
      IF ( (NF .GE. 2 .AND. OP .EQ. 'DISP') .OR.
     +     (NF .GE. 3 .AND. OP .EQ. 'FIND') ) THEN
         CNAME = CLINE(IBF(2):IBF(2)+ILF(2)-1)
         IF ( LISFIL (CNAME) ) THEN
            NEWFIL = .TRUE.
            FILNAM = CNAME
            LINE2 = 0
            COL1 = 1
            COL2 = 80
            FIELD = 3
         ELSE IF (OP .EQ. 'FIND') THEN
            GO TO 1700
         ENDIF
      ENDIF
C
C     --- Assign file (10 sec max wait)
C     DELAY = 0                                                         H
C
C     --- Use function file unit since it is available (unused)
C         at this time.
C
      LFN = IFUN
      IF (LFN.EQ.-1) RETURN
C1000 CALL ASSIGX (LFN, FILNAM, ISTAT)                                  H
 1000 OPEN(UNIT=LFN,FILE=FILNAM,ERR=1010,IOSTAT=ISTAT)                  MLlg
 1010 IF (ISTAT .NE. 0) THEN
C        IF (DELAY .LT. 50) THEN                                        H
C           CALL WAITS (.24)                                            H
C           DELAY = DELAY + 1                                           H
C           GO TO 1000                                                  H
C        ELSE                                                           H
            WRITE (*,*) 'UNABLE TO ASSIGN FILE, '//FILNAM
            CLOSE (UNIT=LFN)                                            MLlg
            RETURN
C        ENDIF                                                          H
      ENDIF
C     OPEN (UNIT=LFN)                                                   H
C
      IF ( NEWFIL ) THEN
         CALL CLINES (LPP)
         LASTLN = NUMLIN (FILNAM)
      ENDIF
C
      IF (OP .EQ. 'FIND') LINE2 = IFND
      IF ( .NOT. UP ) THEN
         LINE1 = MIN (LINE2 + 1, LASTLN)
      ELSE
         LINE1 = MAX (LINE1 - LPP + 4, 1)
      ENDIF
      LINE2 = MIN (LINE1 + LPP - 5, LASTLN)
C
      IF (OP .EQ. 'FIND') THEN
C        --- Check for option character on command
         J = ILINEP + ILF(1) - 1
         I = INDEX (CLINE(ILINEP:J), '.') + ILINEP - 1
         IF (I .GT. ILINEP) THEN
            IF ( INDEX (CLINE(I:J), 'T') .GT. 0 ) THEN
C              --- Start at top of file
               LINE1 = 1
            ELSE IF ( INDEX (CLINE(I:J), 'B') .GT. 0 ) THEN
C              --- Start at bottom of file
               LINE1 = LASTLN
            ENDIF
         ENDIF
         IF (NF .GE. FIELD) THEN
            STRING = CLINE(IBF(FIELD):IBF(FIELD)+ILF(FIELD)-1)
            NC = ILF(FIELD)
         ENDIF
         IF (NC .GT. 0) THEN
            CALL PFIND (LFN, STRING(1:NC), LINE1, UP, FOUND, IDSP)
         ELSE
            FOUND = .FALSE.
            WRITE (IDSP,'(/A)') ' EMPTY STRING'
         ENDIF
C
         IF (FOUND) THEN
            IFND = LINE1
            LINE1 = MAX (LINE1 - 2, 1)
            LINE2 = MIN (LINE1 + LPP - 5, LASTLN)
         ELSE
            IFND = 0
         ENDIF
C
      ELSE IF (NF .GE. FIELD) THEN
C        --- Get line numbers from command
         CNAME = CLINE(IBF(FIELD):IBF(FIELD)+ILF(FIELD)-1)
         J = INDEX (CNAME, '-')
         IF (J .GT. 0) THEN
            NC = J - 1
         ELSE
            NC = ILF(FIELD)
         ENDIF
         IF (NC .GT. 0) THEN
C           --- Get start line number from command
            WRITE (IFMT(3:3),'(I1)') NC
            READ (CNAME(1:NC),IFMT,ERR=1700) L
            IF ( L .GT. 0 ) THEN
               LINE1 = MIN (L, LASTLN)
            ENDIF
         ENDIF
C
         IF (J .GT. 0) THEN
C           --- Get end line number from command
            I = J + 1
            J = ILF(FIELD)
            NC = J - I + 1
            IF (NC .GT. 0) THEN
               WRITE (IFMT(3:3),'(I1)') NC
               READ (CNAME(I:J),IFMT,ERR=1700) L
               IF ( L .GT. 0 ) THEN
                  LINE2 = MIN(L, LASTLN)
               ENDIF
            ENDIF
         ENDIF
C
         FIELD = FIELD + 1
         IF (NF .GE. FIELD) THEN
C           --- Get column numbers from command
            CNAME = CLINE(IBF(FIELD):IBF(FIELD)+ILF(FIELD)-1)
            J = INDEX (CNAME, '-')
            IF (J .GT. 0) THEN
               NC = J - 1
            ELSE
               NC = ILF(FIELD)
            ENDIF
            IF (NC .GT. 0) THEN
C              --- Get start column number from command
               WRITE (IFMT(3:3),'(I1)') NC
               READ (CNAME(1:NC),IFMT,ERR=1700) L
               IF ( L .GT. 0 .AND. L .LE. 133 ) THEN
                  COL1 = L
               ENDIF
            ENDIF
C
            IF (J .GT. 0) THEN
C              --- Get end column number from command
               I = J + 1
               J = ILF(FIELD)
               NC = J - I + 1
               IF (NC .GT. 0) THEN
                  WRITE (IFMT(3:3),'(I1)') NC
                  READ (CNAME(I:J),IFMT,ERR=1700) L
                  IF ( L .GT. 0 .AND. L .LE. 133 ) THEN
                     COL2 = L
                  ENDIF
               ENDIF
            ENDIF
C
C           --- Set minimum width of display
            COL1 = MIN (COL1, COL2-1, 132)
            COL1 = MAX (COL1, 1)
            COL2 = MAX (COL1+1, COL2)
            COL2 = MIN (COL2, 133)
         ENDIF
      ENDIF
C
      IF (FOUND .OR. OP .EQ. 'DISP') THEN
C
         IF (LINE2 .GT. LINE1 + LPP - 5  .OR. LINE2 .LT. LINE1) THEN
            LINE2 = MIN (LINE1 + LPP - 5, LASTLN)
         ENDIF
C
C        --- Position file at line LINE1
         CALL POSFL (LFN, LINE1, ISTAT)                                 MLlg
C        CALL GIOPLW (LFN,'23,IDUM,LINE1-1,ISTAT)                       H
C
C        --- Write page header
         WRITE (LINENO,'(I5,1H-,I5)') LINE1,LINE2
         CALL LJSTR (LINENO,7,5,LINENO,7)
         CALL LJSTR (LINENO,1,11,LINENO,1)
         WRITE (COLNO,'(I3,1H-,I3)') COL1,COL2
         CALL LJSTR (COLNO,5,3,COLNO,5)
         CALL LJSTR (COLNO,1,7,COLNO,1)
         CALL VERASE
         IF(LFRAME) WRITE (IDSP,2000) FILNAM,LINENO,COLNO
C
C        --- Display lines LINE1 through LINE2
         DO 1200 I = LINE1, LINE2
            READ (LFN,'(A)',END=1300) LINE
            CALL CHRLNB (LINE(COL1:COL2),J)
            J = COL1+J
 1120       CONTINUE
            IF (FOUND .AND. I.EQ.IFND) CALL VATT(1)
            WRITE (IDSP,'(1X,A)') LINE(COL1:J)
            IF (FOUND .AND. I.EQ.IFND) CALL VATT(0)
 1200    CONTINUE
         IF (LINE2 .LT. LASTLN .OR. LINE2 .GE. LINE1+LPP-5) THEN
            GO TO 1800
         ENDIF
C
 1300    CONTINUE
         IF(LFRAME) WRITE (IDSP,'(1X,A)') '<END OF FILE>'
      ENDIF
      GO TO 1800
C
 1700 WRITE (IDSP,'(A)') ' INVALID PARAMETER --- '//CNAME
C
 1800 CLOSE (UNIT=LFN)
      RETURN
      END

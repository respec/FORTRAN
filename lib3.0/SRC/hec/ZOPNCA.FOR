      SUBROUTINE ZOPNCA (CDSSFI, ICUNIT, LGENCA, LOPNCA,
     * LCATCA, ICDUNT, LGENCD, LOPNCD, LCATCD, NRECS)
C
C     Generates a Catalog file name (from a DSS file name),
C     then opens the file.  The file is checked to see if
C     it contains a Catalog.
C
C     This routine is for use by those programs accessing
C     the Catalog (see ZCATLG)
C
C     The catalog file name is the name of the DSS file
C     with a "C" added on at the end.  On the PC, the
C     "C" replaces the last character.  E.g.,
C       DSS: DATAB       Catalog: DATABC       (HARRIS)
C       DSS: DATAB.DSS   Catalog: DATAB.DSC    (PC)
C
C     If a vaild Catalog name cannot be formed, or NRECS
C     is -2, a scratch file is used on
C     Harris Computers.  [File W5]
C
C
C     Written by Bill Charley at HEC, 1990.
C
C
      CHARACTER CDSSFI*(*), CCHAR*2, CATFIL*66, CLINE*60, CCOND*11
      LOGICAL LOPEN, LCATLG, LEXIST, LSCRAT, LOPNCD, LCATCD, LCREAT
      LOGICAL LGENCA, LGENCD, LOPNCA, LCATCA, LGEN
      INTEGER*4 NREC, NRECS                                             ML
      INTEGER*4 INTGR                                                   ML
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
      DATA CCOND /' Condensed '/
C
C
C
      IF (MLEVEL.GE.11) THEN                                            D
      WRITE (MUNIT,20) CDSSFI, ICUNIT, ICDUNT, LGENCA, LGENCD           D
 20   FORMAT (T6,'-----DSS---Debug:  Enter ZOPNCA;  DSS File: ',A,/,    D
     * T17,'Catalog Unit:',I4,' Condensed Unit:',I4,/,                  D
     * T17,'Gen Catalog :',L4,' Gen Condensed: ',L4)                    D
      ENDIF                                                             D
C
      IF (LOPNCA) THEN
      CLOSE (UNIT=ICUNIT)
      LOPEN = .FALSE.
      ENDIF
C
      IF (LOPNCD) THEN
      CLOSE (UNIT=ICDUNT)
      LOPNCD = .FALSE.
      ENDIF
C
      LCATCA = .FALSE.
      LCATCD = .FALSE.
      NS = NRECS
      NRECS = -1
      CCHAR = 'CD'                                                      HML
C     CCHAR = 'cd'                                                      u
C
C
      DO 200 J=1,2
C
      CATFIL = CDSSFI
      LGEN = .FALSE.
      LOPEN = .FALSE.
      LCATLG = .FALSE.
C
      IF (J.EQ.1) THEN
      IUNIT = ICUNIT
      LCREAT = LGENCA
      NCOND = 1
      ELSE
      IUNIT = ICDUNT
      LCREAT = LGENCD
      NCOND = 11
      ENDIF
      IF (IUNIT.LE.0) GO TO 180
C
C     IF (NS.EQ.-2) THEN                                                H
C     LSCRAT = .TRUE.                                                   H
C     ELSE                                                              H
      LSCRAT = .FALSE.
      CALL CHRLNB (CATFIL, ILAST)
C     ILAST = ILAST + 1                                                 u
C     CATFIL(ILAST:ILAST) = CCHAR(J:J)                                  u
      I = INDEX(CATFIL,'.')                                             ML
      IF (I.EQ.0) I = ILAST+1                                           ML
      CATFIL(I:I+3) = '.DS' // CCHAR(J:J)                               ML
C     ILAST = ILAST + 1                                                 H
C     CATFIL(ILAST:ILAST) = CCHAR(J:J)                                  H
C     I = INDEX ( CATFIL(1:ILAST), '*')                                 H
C     M = ILAST - I                                                     H
C     IF (M.GE.9) LSCRAT = .TRUE.                                       H
C     ENDIF                                                             H
C
C     IF ((LSCRAT).AND.(J.EQ.2)) GO TO 200                              H
C
C     If not scratch, see if the file exists
      IF (.NOT.LSCRAT) THEN
      CALL CHRLNB (CATFIL, ILAST)
      INQUIRE ( FILE=CATFIL(1:ILAST), EXIST=LEXIST)
      IF (MLEVEL.GE.12) WRITE (MUNIT, 25) LEXIST, CATFIL(1:ILAST)
 25   FORMAT (T17,'File Exists: ',L1,'  Name: -',A,'-')
C
C     If it exists, open it
      IF (LEXIST) THEN
      OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)             HLu
C     OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR,             M
C    * SHARE='DENYNONE')                                                M
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,30) CATFIL(1:ILAST), CCOND(1:NCOND),
     * IERR
 30   FORMAT (/,' **** ZOPNCA - ERROR:  Unable to Access the Catalog',
     * ' File ****',/,' Catalog File: ',A,',  Condensed Catalog: ',A,
     * /,' Error: ',I6)
      IF (.NOT.LCREAT) GO TO 180
      GO TO 180                                                         MLu
C     LSCRAT = .TRUE.                                                   H
C     IF (J.EQ.2) GO TO 200                                             H
      ELSE
      LOPEN = .TRUE.
C
C     Check to see if it is already cataloged
      DO 80 K=1,20
      READ ( IUNIT, 40, END=180) CLINE
 40   FORMAT (A)
      CALL UPCASE (CLINE)
      I = INDEX (CLINE,'HECDSS')
      IF (I.GT.0) THEN
      M = INDEX (CLINE,'CATALOG')
      IF (M.GT.0) THEN
C     LCATLG = .TRUE.
C
      DO 60 N=1,10
      READ ( IUNIT, 40, END=180) CLINE
      CALL UPCASE (CLINE)
C     Check version 6 style catalog
      I = INDEX (CLINE,'OF RECORDS:')
      IF (I.GT.0) THEN
      M = I + 11
      NREC = INTGR (CLINE, M, 7, IER)
      IF (IER.NE.0) GO TO 60
C     Look for a pathanme slash within the next few lines
      DO 50 I=1,10
      READ (IUNIT, 40, END=180) CLINE(1:30)
      CALL UPCASE (CLINE(1:30))
      IF (INDEX(CLINE(1:30),'TAG').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 180
      ENDIF
 50   CONTINUE
      GO TO 180
      ENDIF
C     Check for version 4 style of catalog
      I = INDEX (CLINE,'OF RECORDS =')
      IF (I.GT.0) THEN
      M = I + 12
      NREC = INTGR (CLINE, M, 5, IER)
      IF (IER.NE.0) GO TO 60
      DO 55 I=1,10
      READ (IUNIT, 40, END=180) CLINE
      IF (INDEX(CLINE,'/').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 180
      ENDIF
 55   CONTINUE
      GO TO 180
      ENDIF
 60   CONTINUE
C
      ENDIF
      ENDIF
 80   CONTINUE
      ENDIF
C
      ELSE
      IF (.NOT.LCREAT) GO TO 180
C     CREATE THE CATALOG FILE
      IF (MLEVEL.GE.2) WRITE (MUNIT,90) CCOND(1:NCOND), CATFIL(1:ILAST)
 90   FORMAT (A,'Catalog File Does Not Exist: ',A)
      CALL CCREAT ( CATFIL(1:ILAST), 0, 0, 0, IERR)
      IF (IERR.EQ.0) THEN
C     CALL CRETYP ( CATFIL, '40000164, 0, JERR)                         H
      IF (MLEVEL.GE.2) WRITE (MUNIT,100) CATFIL(1:ILAST)
 100  FORMAT ( ' Created Catalog File: ',A)
      LGEN = .TRUE.
C
C     OPEN THE FILE
      OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)             HLu
C     OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR,             M
C    * SHARE='DENYNONE')                                                M
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,120) CATFIL(1:ILAST), IERR
      LOPEN = .FALSE.
      GO TO 800
      ELSE
      LOPEN = .TRUE.
      ENDIF
      ELSE
      IF (MLEVEL.GE.1) WRITE ( MUNIT, 120) CCOND(1:NCOND),
     * CATFIL(1:ILAST), IERR
 120  FORMAT ( ' --Unable to Create',A,'Catalog File: ',A,I8)
      GO TO 180                                                         MLu
C     LSCRAT = .TRUE.                                                   H
      ENDIF
      ENDIF
      ENDIF
C
C     IF (LSCRAT) THEN                                                  H
C     IF (.NOT.LCREAT) GO TO 800                                        H
C     CATFIL = 'W5 '                                                    H
C     CALL CHRLNB (CATFIL, ILAST)                                       H
C     OPEN ( UNIT=IUNIT, FILE=CATFIL, IOSTAT=IERR)                      H
C
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,120) CATFIL, IERR
      GO TO 800
      ELSE
      LOPEN = .TRUE.
      ENDIF
C     ENDIF                                                             H
C
C
 180  CONTINUE
      IF (LOPEN) REWIND (UNIT=IUNIT)
      IF ((MLEVEL.GE.3).AND.(.NOT.LGEN).AND.LOPEN) THEN
      WRITE (MUNIT, 181) CCOND(1:NCOND), CATFIL(1:ILAST)
 181  FORMAT (A,'Catalog File: ',A)
      ENDIF
C
      IF (J.EQ.1) THEN
      LCATCA = LCATLG
      LOPNCA = LOPEN
      NRECS = NREC
      ELSE
      LCATCD = LCATLG
      LOPNCD = LOPEN
      ENDIF
C
 200  CONTINUE
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT,820) NRECS, LOPNCA, LCATCA,        D
     * LOPNCD, LCATCD                                                   D
 820  FORMAT (T6,'-----DSS---Debug:  Exit ZOPNCA;  NRECS:',I7,/,        D
     * T11,'Catalog File Opened, Cataloged:      ',2L4,/                D
     * T11,'Condensed File Opened, Condensed Cat:',2L4)                 D
      RETURN
C
      END

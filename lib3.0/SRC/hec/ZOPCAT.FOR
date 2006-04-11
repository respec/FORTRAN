      SUBROUTINE ZOPCAT ( CDSSFI, CATFIL, ICUNIT, LOPEN, LCATLG,
     * LCREAT, NREC)
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
C       DSS: datab.dss   Catalog: datab.dssc   (Unix)
C
C     If a vaild Catalog name cannot be formed, or the first two
C     characters of CATFIL are '.T', a scratch file is used on
C     Harris Computers.  [File W5]
C
C     Written by Bill Charley at HEC, 1983.
C
C
      CHARACTER CDSSFI*(*), CATFIL*(*), CLINE*60
      LOGICAL LOPEN, LCATLG, LCREAT, LEXIST, LSCRAT
      INTEGER*4 NREC                                                    ML
      INTEGER*4 INTGR                                                   ML
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      NREC = -1
      LCATLG = .FALSE.
      NDF = LEN ( CDSSFI)
      IF (NDF.GT.66) NDF = 66
      NCF = LEN ( CATFIL)
      IF (NCF.GT.66) NCF = 66
C
      IF (LOPEN) THEN
      CLOSE (UNIT=ICUNIT)
      LOPEN = .FALSE.
      ENDIF
C
C     IF (CATFIL(1:2).EQ.'.T') THEN                                     H
C     LSCRAT = .TRUE.                                                   H
C     ELSE                                                              H
      LSCRAT = .FALSE.
      CATFIL = CDSSFI
      CALL CHRLNB (CATFIL, ILAST)
      I = INDEX(CATFIL,'.')                                             ML
      IF (I.EQ.0) I = ILAST+1                                           ML
      IF (I+3.LE.NCF) THEN                                              ML
      CATFIL(I:I+3) = '.DSC'                                            ML
C     IF (ILAST.LT.NCF) THEN                                            H
C     ILAST = ILAST + 1                                                 H
C     CATFIL(ILAST:ILAST) = 'C'                                         H
C     I = INDEX ( CATFIL(1:ILAST), '*')                                 H
C     J = ILAST - I                                                     H
C     IF (J.GE.9) LSCRAT = .TRUE.                                       H
C     ELSE                                                              H
C     LSCRAT = .TRUE.                                                   H
C     ENDIF                                                             H
      ENDIF                                                             HML
C     ILAST = ILAST + 1                                                 u
C     CATFIL(ILAST:ILAST) = 'c'                                         u
C
C
C     If not scratch, see if the file exists
      IF (.NOT.LSCRAT) THEN
      CALL CHRLNB (CATFIL, ILAST)
      INQUIRE ( FILE=CATFIL(1:ILAST), EXIST=LEXIST)
C
C     If it exists, open it
      IF (LEXIST) THEN
      OPEN ( UNIT=ICUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,20) CATFIL(1:ILAST), IERR
 20   FORMAT (/,' **** ZOPCAT - ERROR:  Unable to Access the Catalog',
     * ' File ****',/,' Catalog File: ',A)
      IF (.NOT.LCREAT) GO TO 820
      GO TO 820                                                         MLu
C     LSCRAT = .TRUE.                                                   H
      ELSE
      LOPEN = .TRUE.
C
C     Check to see if it is already cataloged
      DO 80 K=1,20
      READ ( ICUNIT, 40, END=820) CLINE
 40   FORMAT (A)
      CALL UPCASE (CLINE)
      I = INDEX (CLINE,'HECDSS')
      IF (I.GT.0) THEN
      J = INDEX (CLINE,'CATALOG')
      IF (J.GT.0) THEN
C     LCATLG = .TRUE.
C
      DO 60 N=1,10
      READ ( ICUNIT, 40, END=820) CLINE
      CALL UPCASE (CLINE)
C     Check version 6 style catalog
      I = INDEX (CLINE,'OF RECORDS:')
      IF (I.GT.0) THEN
      J = I + 11
      NREC = INTGR (CLINE, J, 7, IER)
      IF (IER.NE.0) GO TO 60
C     Look for a pathanme slash within the next few lines
      DO 50 I=1,10
      READ (ICUNIT, 40, END=800) CLINE
      IF (INDEX(CLINE,'/').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 800
      ENDIF
 50   CONTINUE
      GO TO 800
      ENDIF
C     Check for version 4 style of catalog
      I = INDEX (CLINE,'OF RECORDS =')
      IF (I.GT.0) THEN
      J = I + 12
      NREC = INTGR (CLINE, J, 5, IER)
      IF (IER.NE.0) GO TO 60
      DO 55 I=1,10
      READ (ICUNIT, 40, END=800) CLINE
      IF (INDEX(CLINE,'/').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 800
      ENDIF
 55   CONTINUE
      GO TO 800
      ENDIF
 60   CONTINUE
C
      ENDIF
      ENDIF
 80   CONTINUE
      ENDIF
C
      ELSE
C     CREATE THE CATALOG FILE
      IF (MLEVEL.GE.2) WRITE (MUNIT,90) CATFIL(1:ILAST)
 90   FORMAT ( ' Catalog File Does Not Exist: ',A)
      IF (.NOT.LCREAT) GO TO 820
      CALL CCREAT ( CATFIL(1:ILAST), 0, 0, 0, IERR)
      IF (IERR.EQ.0) THEN
C     CALL CRETYP ( CATFIL, '40000164, 0, JERR)                         H
      IF (MLEVEL.GE.2) WRITE (MUNIT,100) CATFIL(1:ILAST)
 100  FORMAT ( ' Created Catalog File: ',A)
C
C     OPEN THE FILE
      OPEN ( UNIT=ICUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,20) CATFIL(1:ILAST), IERR
      LOPEN = .FALSE.
      RETURN
      ELSE
      LOPEN = .TRUE.
      ENDIF
      ELSE
      IF (MLEVEL.GE.1) WRITE ( MUNIT, 120) CATFIL(1:ILAST), IERR
 120  FORMAT ( ' --Unable to Create Catalog File: ',A,I8)
      GO TO 820                                                         MLu
C     LSCRAT = .TRUE.                                                   H
      ENDIF
      ENDIF
      ENDIF
C
C     IF (LSCRAT) THEN                                                  H
C     IF (.NOT.LCREAT) RETURN                                           H
C     CATFIL = 'W5 '                                                    H
C     CALL CHRLNB (CATFIL, ILAST)                                       H
C     OPEN ( UNIT=ICUNIT, FILE=CATFIL, IOSTAT=IERR)                     H
C
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,20) CATFIL, IERR
      RETURN
      ELSE
      LOPEN = .TRUE.
      ENDIF
C     ENDIF                                                             H
C
C
 800  CONTINUE
      REWIND ICUNIT
      IF (MLEVEL.GE.3) WRITE ( MUNIT, 801) CATFIL(1:ILAST)
 801  FORMAT ( ' Catalog File: ',A)
C
 820  CONTINUE
      RETURN
C
      END

*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/tonew.f,v 1.1 1998/07/07 20:20:48 grogers Exp $
*  tonew.f
*
      SUBROUTINE TONEW (KIN, KOUT, XMIN, XMAX, YMIN, YMAX,
     +                  NCFRST, NCLAST, NRFRST, NRLAST, FOUTA, FOUTO,
     +                  IMETR)

*  Purpose:  Copy from input files to output files
**************************************************
*
*  $Log: tonew.f,v $
*  Revision 1.1  1998/07/07 20:20:48  grogers
*  PR#0, initial add of nadgrd_lib
*
*

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* NMAX is the maximum size of the buffer.  It is at least as large
* as the largest number of columns in the input grid

      INTEGER NMAX
      PARAMETER (NMAX = 1024)

* Variables ending in an 'A' are associated with the latitude grid
* files while variables ending in an 'O' are associated with the
* longitude grid files.

      DOUBLE PRECISION LAT, LON, DLAT, DLON
      REAL XMIN, XMAX, YMIN, YMAX
      REAL ANGLE, SCR
      REAL DLATM, DLONM
      REAL BUFA(NMAX), BUFO(NMAX)
      INTEGER NCFRST, NCLAST, NRFRST, NRLAST, IMETR
      INTEGER NZ, LRECL, KIN, KOUT, NRPR1, NRPR2
      INTEGER IFLAG2, N2
      INTEGER I, I2, IDUM, IREC1, IREC2, J, JJ, J1, J2, NR2, NC2
      CHARACTER*56 RIDENT
      CHARACTER*32 FOUTA, FOUTO
      CHARACTER*8 PGM
      CHARACTER*1 ASCR

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      REAL XMIN0, XMAX0, YMIN0, YMAX0, DX, DY
      INTEGER NC, NR
      COMMON /GRID0/ XMIN0, XMAX0, YMIN0, YMAX0, DX, DY, NR, NC

      DOUBLE PRECISION ZSUMA, ZSUMO, ZSQRA, ZSQRO
      REAL ZMINA, ZMINO, ZMAXA, ZMAXO
      COMMON /ZSTATS/ ZMINA, ZMAXA, ZMINO, ZMAXO,
     +                ZSUMA, ZSQRA, ZSUMO, ZSQRO

      DOUBLE PRECISION ZMSUMA, ZMSUMO, ZMSQRA, ZMSQRO
      REAL ZMMINA, ZMMINO, ZMMAXA, ZMMAXO
      COMMON /ZMSTAT/ ZMMINA, ZMMAXA, ZMMINO, ZMMAXO,
     +                ZMSUMA, ZMSQRA, ZMSUMO, ZMSQRO

      DATA IFLAG2 /2/

* Open output file, write header information, reopen for grid size

      ANGLE = 0.0
      RIDENT = 'NADCON EXTRACTED REGION'
      PGM = 'NADGRD'
      NZ = 1
      NR2 = NRLAST - NRFRST + 1
      NC2 = NCLAST - NCFRST + 1

      IF (KOUT .EQ. 1) THEN

* Binary output files

        LRECL = 4*(NC2 + 1)
        OPEN (NOUTA,FILE=FOUTA,FORM='UNFORMATTED',ACCESS='DIRECT',
     +        RECL=LRECL,STATUS='UNKNOWN')
        OPEN (NOUTO,FILE=FOUTO,FORM='UNFORMATTED',ACCESS='DIRECT',
     +        RECL=LRECL,STATUS='UNKNOWN')
        WRITE (NOUTA,REC=1) RIDENT, PGM, NC2, NR2, NZ, XMIN, DX,
     +                      YMIN, DY, ANGLE
        WRITE (NOUTO,REC=1) RIDENT, PGM, NC2, NR2, NZ, XMIN, DX,
     +                      YMIN, DY, ANGLE

      ELSEIF (KOUT .EQ. -1) THEN

* ASCII transfer format output files

        OPEN (NOUTA,FILE=FOUTA,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        STATUS='UNKNOWN')
        OPEN (NOUTO,FILE=FOUTO,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        STATUS='UNKNOWN')
        WRITE (NOUTA,140) RIDENT, PGM
        WRITE (NOUTO,140) RIDENT, PGM
  140   FORMAT (A56, A8)
        WRITE (NOUTA,150) NC2, NR2, NZ, XMIN, DX,
     +                    YMIN, DY, ANGLE
        WRITE (NOUTO,150) NC2, NR2, NZ, XMIN, DX,
     +                    YMIN, DY, ANGLE
  150   FORMAT (3I4, 5F12.5)

      ELSEIF (KOUT .EQ. -2) THEN

* ASCII graphics format output files (SURFER header information)

        OPEN (NOUTA,FILE=FOUTA,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        STATUS='UNKNOWN')
        OPEN (NOUTO,FILE=FOUTO,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        STATUS='UNKNOWN')
        WRITE (NOUTA,160)
        WRITE (NOUTO,160)
  160   FORMAT ('DSAA')
        WRITE (NOUTA,165) NC2, NR2
        WRITE (NOUTO,165) NC2, NR2
  165   FORMAT (2I12)
        WRITE (NOUTA,170) XMIN, XMAX
        WRITE (NOUTO,170) XMIN, XMAX
  170   FORMAT (2F12.4)
        WRITE (NOUTA,170) YMIN, YMAX
        WRITE (NOUTO,170) YMIN, YMAX
        IF (IMETR .EQ. 1) THEN
          WRITE (NOUTA,170) ZMINA, ZMAXA
          WRITE (NOUTO,170) ZMINO, ZMAXO
        ELSE
          WRITE (NOUTA,170) ZMMINA, ZMMAXA
          WRITE (NOUTO,170) ZMMINO, ZMMAXO
        ENDIF
      ENDIF

      CALL NBLANK (FOUTA, IFLAG2, N2)
      WRITE (LUOUT,80) FOUTA(1:N2), FOUTO(1:N2)
   80 FORMAT (/, ' NADGRD is now creating the ''', A, ''' and ''', A,
     +           ''' files.',
     +        /, ' This process may take several minutes.')

* number of ASCII rows per binary row (note that there are always
* an odd number of grid columns)

        IF (KIN .EQ. -1) THEN
          NRPR1 = NC/6 + 1

* For ASCII input files, skip up to the NRFRST equivalent row

          REWIND NINA
          REWIND NINO
          READ (NINA, '(A1)') ASCR
          READ (NINO, '(A1)') ASCR
          READ (NINA, '(A1)') ASCR
          READ (NINO, '(A1)') ASCR
          IF (NRFRST .GT. 1) THEN
            I2 = (NRFRST - 1)*NRPR1
            DO 210 I = 1, I2
              READ (NINA, 200) SCR
              READ (NINO, 200) SCR
  200         FORMAT (F12.6)
  210       CONTINUE
          ENDIF

        ENDIF

        IF (KOUT .EQ. -1) THEN
          NRPR2 = (NCLAST - NCFRST + 1)/6 + 1
        ELSEIF (KOUT .EQ. 1) THEN
          IREC2 = 1
        ENDIF

* Copy input files to output files

      DO 300 I = NRFRST, NRLAST

* read input file

        IF (KIN .EQ. 1) THEN
          IREC1 = I + 1
          READ (NINA,REC=IREC1) IDUM, (BUFA(J), J = 1, NC)
          READ (NINO,REC=IREC1) IDUM, (BUFO(J), J = 1, NC)
        ELSE
          DO 320 JJ = 1, NRPR1-1
            J1 = (JJ-1)*6 + 1
            J2 = J1 + 5
            READ (NINA, 310) (BUFA(J), J = J1, J2)
            READ (NINO, 310) (BUFO(J), J = J1, J2)
  310       FORMAT (6F12.6)
  320     CONTINUE
          J1 = J2 + 1
          READ (NINA, 310) (BUFA(J), J = J1, NC)
          READ (NINO, 310) (BUFO(J), J = J1, NC)
        ENDIF

* write output file

        IF (KOUT .EQ. 1) THEN
          IDUM = 0
          IREC2 = IREC2 + 1
          WRITE (NOUTA,REC=IREC2) IDUM, (BUFA(J), J = NCFRST, NCLAST)
          WRITE (NOUTO,REC=IREC2) IDUM, (BUFO(J), J = NCFRST, NCLAST)
        ELSEIF (KOUT .EQ. -1) THEN
          DO 420 JJ = 1, NRPR2-1
            J1 = (JJ-1)*6 + NCFRST
            J2 = J1 + 5
            WRITE (NOUTA, 310) (BUFA(J), J = J1, J2)
            WRITE (NOUTO, 310) (BUFO(J), J = J1, J2)
  420     CONTINUE
          J1 = J2 + 1
          WRITE (NOUTA, 310) (BUFA(J), J = J1, NCLAST)
          WRITE (NOUTO, 310) (BUFO(J), J = J1, NCLAST)
        ELSEIF (KOUT .EQ. -2) THEN
          IF (IMETR .EQ. -1) THEN

*** If the output graphics grids are to be in meters, then
*** translate the buffer contents

            LAT = (DBLE( I) - 1.D0)*DBLE(DY) + DBLE(YMIN0)
            DO 430 JJ = NCFRST, NCLAST
              LON = (DBLE(JJ) - 1.D0)*DBLE(DX) + DBLE(XMIN0)
              DLON = DBLE( BUFO(JJ) )
              DLAT = DBLE( BUFA(JJ) )
              CALL METER2 (LAT, LON, DLAT, DLON, DLATM, DLONM)
              BUFO(JJ) = DLONM
              BUFA(JJ) = DLATM
  430       CONTINUE
          ENDIF
          WRITE (NOUTA,440) (BUFA(J), J = NCFRST, NCLAST)
          WRITE (NOUTO,440) (BUFO(J), J = NCFRST, NCLAST)
  440     FORMAT (1024F12.6)
        ENDIF

  300 CONTINUE

      IF (KOUT .EQ. 1) THEN
        CALL NBLANK (FOUTA, IFLAG2, N2)
        WRITE (LUOUT,130) FOUTA(1:N2), FOUTO(1:N2)
  130   FORMAT (/, ' Binary files ''', A, ''' and ''', A, '''',
     +             ' have been created.')
      ELSEIF (KOUT .EQ. -1  .OR.  KOUT .EQ. -2) THEN
        CALL NBLANK (FOUTA, IFLAG2, N2)
        WRITE (LUOUT,135) FOUTA(1:N2), FOUTO(1:N2)
  135   FORMAT (/, ' ASCII files ''', A, ''' and ''', A, '''',
     +             ' have been created.')
      ENDIF

      RETURN
      END

*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/zextrm.f,v 1.1 1998/07/07 20:20:49 grogers Exp $
*  zextrm.f
*
      SUBROUTINE ZEXTRM (KIN, KOUT, NCFRST, NCLAST, NRFRST, NRLAST)

* Purpose: Read from the input file to obtain the minimum and maximum Z
* values for the new grids, and other statistical values
***********************************************************************
*
*  $Log: zextrm.f,v $
*  Revision 1.1  1998/07/07 20:20:49  grogers
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

      DOUBLE PRECISION AVEA, AVEO, SIGA, SIGO
      DOUBLE PRECISION AVEAM, AVEOM, SIGAM, SIGOM
      REAL BUFA(NMAX), BUFO(NMAX)
      REAL SCR
      INTEGER I2, NRPR1, JJ, J1, J2, NTOT
      INTEGER IREC, I, J, IDUM, KIN, KOUT, ILAST
      INTEGER NCFRST, NCLAST, NRFRST, NRLAST
      CHARACTER*1 ASCR

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      DOUBLE PRECISION ZSUMA, ZSUMO, ZSQRA, ZSQRO
      REAL ZMINA, ZMINO, ZMAXA, ZMAXO
      COMMON /ZSTATS/ ZMINA, ZMAXA, ZMINO, ZMAXO,
     +                ZSUMA, ZSQRA, ZSUMO, ZSQRO

      DOUBLE PRECISION ZMSUMA, ZMSUMO, ZMSQRA, ZMSQRO
      REAL ZMMINA, ZMMINO, ZMMAXA, ZMMAXO
      COMMON /ZMSTAT/ ZMMINA, ZMMAXA, ZMMINO, ZMMAXO,
     +                ZMSUMA, ZMSQRA, ZMSUMO, ZMSQRO

      REAL XMIN0, XMAX0, YMIN0, YMAX0, DX, DY
      INTEGER NC, NR
      COMMON /GRID0/ XMIN0, XMAX0, YMIN0, YMAX0, DX, DY, NR, NC

* Initialize

      ZMINA = 9.9E10
      ZMAXA = -9.9E10
      ZMINO = 9.9E10
      ZMAXO = -9.9E10
      ZSUMA = 0.D0
      ZSQRA = 0.D0
      ZSUMO = 0.D0
      ZSQRO = 0.D0

      ZMMINA = 9.9E10
      ZMMAXA = -9.9E10
      ZMMINO = 9.9E10
      ZMMAXO = -9.9E10
      ZMSUMA = 0.D0
      ZMSQRA = 0.D0
      ZMSUMO = 0.D0
      ZMSQRO = 0.D0

      NTOT = 0

      IF (KOUT .NE. 0) THEN
        WRITE (LUOUT,80)
   80   FORMAT (/, ' NADGRD is now finding the maximum and minimum',
     +             ' shift values in the new grids.',
     +          /, ' This process may take several minutes.')
      ELSE
        WRITE (LUOUT,85)
   85   FORMAT (/, ' NADGRD is now finding the maximum and minimum',
     +             ' shift values in the input grids.',
     +          /, ' This process may take several minutes.')
      ENDIF

      IF (KIN .EQ. 1) THEN
        DO 300 I = NRFRST, NRLAST
          IREC = I + 1

          READ (NINA,REC=IREC) IDUM, (BUFA(J), J = 1, NC)
          READ (NINO,REC=IREC) IDUM, (BUFO(J), J = 1, NC)

          CALL ZSUMS (I, NCFRST, NCLAST, BUFA, BUFO)
          NTOT = NTOT + NCLAST - NCFRST + 1

  300   CONTINUE

      ELSEIF (KIN .EQ. -1) THEN

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
  200       FORMAT (F12.6)
  210     CONTINUE
        ENDIF

        ILAST = NRLAST - NRFRST + 1
        DO 340 I = 1, ILAST
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
          CALL ZSUMS (I, NCFRST, NCLAST, BUFA, BUFO)
          NTOT = NTOT + NCLAST - NCFRST + 1
  340   CONTINUE

      ENDIF

      AVEA = ZSUMA / NTOT
      AVEO = ZSUMO / NTOT
      SIGA = DSQRT(  ( DBLE(NTOT)*ZSQRA - ZSUMA*ZSUMA )/
     +       ( DBLE(NTOT)*DBLE(NTOT-1) )  )
      SIGO = DSQRT(  ( DBLE(NTOT)*ZSQRO - ZSUMO*ZSUMO )/
     +       ( DBLE(NTOT)*DBLE(NTOT-1) )  )

      AVEAM = ZMSUMA / NTOT
      AVEOM = ZMSUMO / NTOT
      SIGAM = DSQRT(  ( DBLE(NTOT)*ZMSQRA - ZMSUMA*ZMSUMA )/
     +       ( DBLE(NTOT)*DBLE(NTOT-1) )  )
      SIGOM = DSQRT(  ( DBLE(NTOT)*ZMSQRO - ZMSUMO*ZMSUMO )/
     +       ( DBLE(NTOT)*DBLE(NTOT-1) )  )

      WRITE (LUOUT, 500)
  500 FORMAT (/, 32X, 'SHIFT STATISTICS',
     +        /, 1X, 71('-'),
     +        /, 18X, 2(5X, 'Latitude', 5X, 'Longitude'),
     +        /, 27X, '(seconds of arc)', 13X, '(meters)')
      WRITE (LUOUT, 510) ZMINA, ZMINO, ZMMINA, ZMMINO
  510 FORMAT (' Minimum', 14X, F9.3, 5X, F9.3, 4X, F9.3, 5X, F9.3)
      WRITE (LUOUT, 520) ZMAXA, ZMAXO, ZMMAXA, ZMMAXO
  520 FORMAT (' Maximum', 14X, F9.3, 5X, F9.3, 4X, F9.3, 5X, F9.3)
      WRITE (LUOUT, 530) AVEA, AVEO, AVEAM, AVEOM
  530 FORMAT (/, ' Average', 14X, F9.3, 5X, F9.3, 4X, F9.3, 5X, F9.3)
      WRITE (LUOUT, 540) SIGA, SIGO, SIGAM, SIGOM
  540 FORMAT (' Standard Deviation', 3X, F9.3, 5X, F9.3, 4X, F9.3, 5X,
     +                                                             F9.3)

      IF (KOUT .EQ. 0) THEN
        WRITE (NOUTA, 500)
        WRITE (NOUTA, 510) ZMINA, ZMINO, ZMMINA, ZMMINO
        WRITE (NOUTA, 520) ZMAXA, ZMAXO, ZMMAXA, ZMMAXO
        WRITE (NOUTA, 530) AVEA, AVEO, AVEAM, AVEOM
        WRITE (NOUTA, 540) SIGA, SIGO, SIGAM, SIGOM
        WRITE (LUOUT,*)
      ENDIF

      RETURN
      END

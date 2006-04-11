*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/zsums.f,v 1.1 1998/07/07 20:20:50 grogers Exp $
*  zsums.f
*
      SUBROUTINE ZSUMS (I, NCFRST, NCLAST, BUFA, BUFO)

* Purpose: Do the summations for the subroutine ZEXTRM
******************************************************
*
*  $Log: zsums.f,v $
*  Revision 1.1  1998/07/07 20:20:50  grogers
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
      REAL BUFA(NMAX), BUFO(NMAX)
      REAL ZA, ZO, DLATM, DLONM
      INTEGER K, I, NCFRST, NCLAST

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

      DO 400 K = NCFRST, NCLAST

*** seconds of arc statistics

        ZA = BUFA(K)
        ZO = BUFO(K)
        ZMINA = MIN(ZMINA, ZA)
        ZMAXA = MAX(ZMAXA, ZA)
        ZMINO = MIN(ZMINO, ZO)
        ZMAXO = MAX(ZMAXO, ZO)
        ZSUMA = ZSUMA + DBLE(ZA)
        ZSQRA = ZSQRA + DBLE(ZA*ZA)
        ZSUMO = ZSUMO + DBLE(ZO)
        ZSQRO = ZSQRO + DBLE(ZO*ZO)

*** now meters

        LON = (DBLE(K) - 1.D0)*DBLE(DX) + DBLE(XMIN0)
        LAT = (DBLE(I) - 1.D0)*DBLE(DY) + DBLE(YMIN0)
        DLON = DBLE(ZO)
        DLAT = DBLE(ZA)
        CALL METER2 (LAT, LON, DLAT, DLON, DLATM, DLONM)
        ZMMINA = MIN(ZMMINA, DLATM)
        ZMMAXA = MAX(ZMMAXA, DLATM)
        ZMMINO = MIN(ZMMINO, DLONM)
        ZMMAXO = MAX(ZMMAXO, DLONM)
        ZMSUMA = ZMSUMA + DBLE(DLATM)
        ZMSQRA = ZMSQRA + DBLE(DLATM*DLATM)
        ZMSUMO = ZMSUMO + DBLE(DLONM)
        ZMSQRO = ZMSQRO + DBLE(DLONM*DLONM)

  400 CONTINUE

      RETURN
      END

*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/print2.f,v 1.1 1998/07/07 19:32:31 grogers Exp $
*  print2.f
*
      SUBROUTINE PRINT2 (LU, NCONV, NAME, VRSION, IDLA, IMLA, SLA,
     +                   IDLO, IMLO, SLO, IDLA2, IMLA2, SLA2, IDLO2,
     +                   IMLO2, SLO2, KEY, IFMT,dsel)

* Purpose: Prints out the actual transformation results
*******************************************************
*
*  $Log: print2.f,v $
*  Revision 1.1  1998/07/07 19:32:31  grogers
*  PR#0, initial load of nadcon_lib
*
*
* This subroutine prints out the actual transformation results using
* a free format - the same as the input file format.  This is used
* for type 2 format.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION DDLA, DDLO, DMLA, DMLO
      DOUBLE PRECISION DDLA2, DDLO2, DMLA2, DMLO2
      DOUBLE PRECISION VRSION
      DOUBLE PRECISION SLA, SLO, SLA2, SLO2
      INTEGER LU, NCONV, KEY, IFMT
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IDLA2, IMLA2, IDLO2, IMLO2
      CHARACTER*80 NAME
      LOGICAL      dsel

* Write header record to identify source of coordinates and datum

      IF (NCONV .EQ. 1) THEN
       if(dsel) then
c nad 27, nad 83
        IF (KEY .EQ. 1) THEN
          WRITE (LU, 10) VRSION
   10     FORMAT (' NADCON Version', F5.2,
     +      ' - NAD 83 datum values converted from NAD 27 datum values')
        ELSE
          WRITE (LU, 20) VRSION
   20     FORMAT (' NADCON Version', F5.2,
     +      ' - NAD 27 datum values converted from NAD 83 datum values')
        ENDIF
       else
c nad 83, hpgn
        IF (KEY .EQ. 1) THEN
          WRITE (LU, 11) VRSION
   11     FORMAT (' NADCON Version', F5.2,
     +      ' - HPGN datum values converted from NAD 83 datum values')
        ELSE
          WRITE (LU, 21) VRSION
   21     FORMAT (' NADCON Version', F5.2,
     +      ' - NAD 83 datum values converted from HPGN datum values')
        ENDIF
       end if
      ENDIF

* Write transformed coordinates

      IF (IFMT .EQ. 1) THEN
        IF (KEY .EQ. 1) THEN
          WRITE (LU, 110) IDLA2, IMLA2, SLA2, IDLO2, IMLO2, SLO2, NAME
  110     FORMAT (I4, I3, F9.5, I4, I3, F9.5, 8X, A40)
        ELSE
          WRITE (LU, 110) IDLA, IMLA, SLA, IDLO, IMLO, SLO, NAME
        ENDIF
      ELSEIF (IFMT .EQ. 2) THEN
        IF (KEY .EQ. 1) THEN
          DMLA2 = DBLE(IMLA2) + SLA2/60.D0
          DMLO2 = DBLE(IMLO2) + SLO2/60.D0
          WRITE (LU, 120) IDLA2, DMLA2, IDLO2, DMLO2, NAME
  120     FORMAT (I4, F11.7, 1X, I4, F11.7, 1X, 8X, A40)
        ELSE
          DMLA = DBLE(IMLA) + SLA/60.D0
          DMLO = DBLE(IMLO) + SLO/60.D0
          WRITE (LU, 120) IDLA, DMLA, IDLO, DMLO, NAME
        ENDIF
      ELSEIF (IFMT .EQ. 3) THEN
        IF (KEY .EQ. 1) THEN
          DDLA2 = DBLE(IDLA2) + DBLE(IMLA2)/60.D0 + SLA2/3600.D0
          DDLO2 = DBLE(IDLO2) + DBLE(IMLO2)/60.D0 + SLO2/3600.D0
          WRITE (LU, 130) DDLA2, DDLO2, NAME
  130     FORMAT (F14.9, 2X, F14.9, 2X, 8X, A40)
        ELSE
          DDLA = DBLE(IDLA) + DBLE(IMLA)/60.D0 + SLA/3600.D0
          DDLO = DBLE(IDLO) + DBLE(IMLO)/60.D0 + SLO/3600.D0
          WRITE (LU, 130) DDLA, DDLO, NAME
        ENDIF
      ENDIF

      RETURN
      END

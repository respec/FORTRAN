*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/wrtpt.f,v 1.1 1998/07/07 19:32:42 grogers Exp $
*  wrtpt.f
*
      SUBROUTINE WRTPT (ITYPE, KEY, NCONV, VRSION, NAME,
     +                  IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                  IDLA2, IMLA2, SLA2, IDLO2, IMLO2, SLO2,
     +                  DLAM, DLOM, DLAS, DLOS, IFMT,
     +                  FIRST, LAST, IPREC, RESP, IPAGE, PAGE, SCREEN,
     +                  dsel)

*  Purpose:  Write the NAD 83, NAD 27, and shift values to output file
*            (and screen).
**********************************************************************
*
*  $Log: wrtpt.f,v $
*  Revision 1.1  1998/07/07 19:32:42  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION VRSION
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SLA, SLO, SLA2, SLO2
      INTEGER  IDLA, IMLA, IDLO, IMLO
      INTEGER  IDLA2, IMLA2, IDLO2, IMLO2
      INTEGER ITYPE, KEY, NCONV, IFMT, IPREC, IPAGE
      CHARACTER*80 NAME
      CHARACTER*44 FIRST
      CHARACTER*30 LAST
      CHARACTER*15 RESP
      LOGICAL PAGE, SCREEN,dsel

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)
*********************
* PAGE NUMBER COUNTER
*********************

* this is where you change how many on a page

        IF ( MOD(NCONV,5) .EQ. 0  .OR.  NCONV .EQ. 1) THEN
          PAGE = .TRUE.
          IPAGE = IPAGE + 1
        ENDIF

*********************************
** WRITE TO OUTPUT FILE OR SCREEN
*********************************

        IF (ITYPE .EQ. 0) THEN

**************************************
* ONLY INTERACTIVE USE - NO INPUT FILE
**************************************
          CALL PRINT1 (NOUT, NCONV, NAME, VRSION, IDLA, IMLA, SLA,
     +                 IDLO, IMLO, SLO, IDLA2, IMLA2, SLA2, IDLO2,
     +                 IMLO2, SLO2, DLAM, DLOM, DLAS, DLOS, RESP,
     +                IPAGE, PAGE, KEY,dsel)

        ELSEIF (ITYPE .EQ. 1) THEN

**************************
* FOR FREE FORMAT TYPE 1
**************************
          CALL PRINT1 (NOUT, NCONV, NAME, VRSION, IDLA, IMLA, SLA,
     +                 IDLO, IMLO, SLO, IDLA2, IMLA2, SLA2, IDLO2,
     +                 IMLO2, SLO2, DLAM, DLOM, DLAS, DLOS, RESP,
     +                 IPAGE, PAGE, KEY,dsel)
        ELSEIF (ITYPE .EQ. 2) THEN

**************************
* FOR FREE FORMAT TYPE 2
**************************

          CALL PRINT2 (NOUT, NCONV, NAME, VRSION, IDLA, IMLA, SLA,
     +                 IDLO, IMLO, SLO, IDLA2, IMLA2, SLA2, IDLO2,
     +                 IMLO2, SLO2, KEY, IFMT,dsel)

        ELSEIF (ITYPE .EQ. 3) THEN

****************************************
* FOR INPUT FILE ITYPE = 3
* THE HORIZONTAL BLUE BOOK SPECIFICATION
****************************************

          CALL PRINT3 (NOUT, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +                 IDLA2, IMLA2, SLA2, IDLO2, IMLO2, SLO2,
     +                 KEY, FIRST, LAST, IPREC)

        ENDIF

*******************
* FOR SCREEN OUTPUT
*******************

        IF (SCREEN) THEN
          IF (ITYPE .EQ. 3) NAME = FIRST(15:44)
          CALL PRINT1 (LUOUT, NCONV, NAME, VRSION, IDLA, IMLA, SLA,
     +                 IDLO, IMLO, SLO, IDLA2, IMLA2, SLA2, IDLO2,
     +                 IMLO2, SLO2, DLAM, DLOM, DLAS, DLOS, RESP,
     +                IPAGE, PAGE, KEY,dsel)
        ENDIF

      RETURN
      END

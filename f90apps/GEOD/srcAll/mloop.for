*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/mloop.f,v 1.1 1998/07/07 19:32:26 grogers Exp $
*  mloop.f
*
      SUBROUTINE MLOOP (NCONV, IPAGE, ITYPE, KEY, VRSION,
     +                  DLAM, DLOM, DLAS, DLOS,
     +                  SDLAM, SDLAM2, SDLOM, SDLOM2,
     +                  SDLAS, SDLAS2, SDLOS, SDLOS2,
     +                  SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                  SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                  XSMALL, XBIG, YSMALL, YBIG,
     +                  PAGE, SCREEN,dsel)

**********************************************************************
* Purpose: LOOPS THROUGH THE INPUT DATA (EITHER AN INPUT DATA *
* FILE OR INTERACTIVELY), CALCULATES THE TRANSFORMATION VALUES,      *
* UPDATES THE MINIMUM, MAXIMUM, AND STATISTICAL SUMMATIONS, AND THEN *
* PRINTS THE RESULTS TO THE OUTPUT FILE AND/OR THE SCREEN.           *
**********************************************************************

*
*  $Log: mloop.f,v $
*  Revision 1.1  1998/07/07 19:32:26  grogers
*  PR#0, initial load of nadcon_lib
*
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION DLAM2, DLOM2, DLAS2, DLOS2
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG, XPT, XPT2, YPT, YPT2
      DOUBLE PRECISION VRSION
      DOUBLE PRECISION SLA, SLO, SLA2, SLO2
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER NCONV, IPAGE, ITYPE, KEY, IFMT, IPREC
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IDLA2, IMLA2, IDLO2, IMLO2
      CHARACTER*80 NAME
      CHARACTER*44 FIRST
      CHARACTER*30 LAST
      CHARACTER*15 RESP
      LOGICAL PAGE, NOGO, SCREEN, NOPT, EOF,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* set defaults for those variables not used by every format type

      DATA NAME /' '/, FIRST /' '/, LAST /' '/, IFMT /0/

*******************************************************************
* BEGIN THE COMPUTATION LOOP FOR EACH CONVERSION
* DO UNTIL END OF FILE OR NO MORE CONVERSIONS REQUESTED
*******************************************************************

      NCONV = 0
  160 CONTINUE

        PAGE = .FALSE.

********************************************
* GET THE NAME AND LOCATION OF ANOTHER POINT
********************************************

        CALL GETPT (NCONV, ITYPE, KEY, NAME, IDLA, IMLA, SLA,
     +              IDLO, IMLO, SLO, XPT, YPT,
     +              EOF, NOPT, FIRST, LAST, IPREC, IFMT,dsel)
        IF (NOPT) GOTO 155
        IF (EOF) GOTO 9999

************************
* DO THE TRANSFORMATION
************************
        NOGO = .FALSE.
        CALL TRANSF (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +               DLAM, DLOM, DLAS, DLOS, KEY, ITYPE)

****************************************************
* CHECK TO SEE IF THIS POINT CAN BE TRANSFORMED
* IF NOGO IS TRUE THEN GET ANOTHER POINT AND DON'T
* DO THE COMPUTATION - POINT IS OUT OF BOUNDS
* IF NOGO IS NOT TRUE THEN PROCEED - ESTIMATE MADE
****************************************************

        IF (NOGO) GOTO 155
        NCONV = NCONV + 1

********************************************
* CHANGE THE LONGITUDE BACK TO POSITIVE WEST
* AND CHANGE BACK TO D.M.S FORMAT
********************************************

        XPT2 = -XPT2

        IF (KEY .EQ. 1) THEN

**********************
* FOR NAD 27 TO NAD 83
**********************

          CALL HMS (YPT2, IDLA2, IMLA2, SLA2)
          CALL HMS (XPT2, IDLO2, IMLO2, SLO2)
        ELSEIF (KEY .EQ. -1) THEN

**********************
* FOR NAD 83 TO NAD 27
**********************

          XPT = -XPT
          IDLA2 = IDLA
          IMLA2 = IMLA
          SLA2 = SLA
          IDLO2 = IDLO
          IMLO2 = IMLO
          SLO2 = SLO
          CALL HMS (YPT, IDLA, IMLA, SLA)
          CALL HMS (XPT, IDLO, IMLO, SLO)
        ENDIF

**************************
* DO THE LITTLE STATISTICS
**************************

* First, the basics
* meters....

        DLAM2  = DLAM**2
        SDLAM2 = SDLAM2 + DLAM2
        SDLAM  = SDLAM  + DLAM

        DLOM2  = DLOM**2
        SDLOM2 = SDLOM2 + DLOM2
        SDLOM  = SDLOM  + DLOM

* seconds....

        DLAS2  = DLAS**2
        SDLAS2 = SDLAS2 + DLAS2
        SDLAS  = SDLAS  + DLAS

        DLOS2  = DLOS**2
        SDLOS2 = SDLOS2 + DLOS2
        SDLOS  = SDLOS  + DLOS

* then the ranges

        XSMALL = DMIN1( XSMALL, XPT)
        XBIG   = DMAX1( XBIG  , XPT)
        YSMALL = DMIN1( YSMALL, YPT)
        YBIG   = DMAX1( YBIG  , YPT)

        SMDLAM = DMIN1( SMDLAM, DLAM)
        BGDLAM = DMAX1( BGDLAM, DLAM)

        SMDLOM = DMIN1( SMDLOM, DLOM)
        BGDLOM = DMAX1( BGDLOM, DLOM)

        SMDLAS = DMIN1( SMDLAS, DLAS)
        BGDLAS = DMAX1( BGDLAS, DLAS)

        SMDLOS = DMIN1( SMDLOS, DLOS)
        BGDLOS = DMAX1( BGDLOS, DLOS)

**********************************
** WRITE TO OUTPUT FILE AND SCREEN
**********************************
        CALL WRTPT (ITYPE, KEY, NCONV, VRSION, NAME,
     +              IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              IDLA2, IMLA2, SLA2, IDLO2, IMLO2, SLO2,
     +              DLAM, DLOM, DLAS, DLOS, IFMT,
     +              FIRST, LAST, IPREC, RESP, IPAGE, PAGE, SCREEN,
     +              dsel)

**********************
* START THE LOOP AGAIN
**********************

  155 GOTO 160

 9999 RETURN
      END

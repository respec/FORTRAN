*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/initl.f,v 1.1 1998/07/07 19:32:20 grogers Exp $
*  initl.f
*
      SUBROUTINE INITL (SCREEN, PAGE, IPAGE, ITYPE,
     +                  SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                  SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                  ADLAM, VDLAM, SDLAM, SDLAM2,
     +                  ADLOM, VDLOM, SDLOM, SDLOM2,
     +                  ADLAS, VDLAS, SDLAS, SDLAS2,
     +                  ADLOS, VDLOS, SDLOS, SDLOS2,
     +                  XSMALL, XBIG, YSMALL, YBIG,dsel)

*  Purpose: Initializes all the variables needed in NADCON
**********************************************************
*
*  $Log: initl.f,v $
*  Revision 1.1  1998/07/07 19:32:20  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*20 B20
      CHARACTER*80 B80
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER IPAGE, ITYPE
      LOGICAL PAGE, SCREEN, dsel

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* Initialize card variable in common CURNT to blank

      CARD = B80

* Set the logical units for input/output common INOUT
* Note that the unit numbers for the data grids (LUAREA) are defined
* in subroutine OPENFL as the numbers 11 through 2*MXAREA

      LUIN  = 5
      LUOUT = 6
*GR   NOUT  = 101
*GR   NIN   = 102
*GR   NAPAR = 103
      NOUT  = 1
      NIN   = 2
      NAPAR = 3

******************************************************************
*                             INITIALIZE
******************************************************************

* Defaults: SCREEN = .TRUE. => send results to screen
*           PAGE = .FALSE.  => don't start a new page in the output file
*           IPAGE = 0       => current output file page number is 0
*           ITYPE = 0       => interactive input of points
*           dsel  = .FALSE. => select NAD 83, HPGN datum conversion

      SCREEN = .TRUE.
      PAGE = .FALSE.
      IPAGE = 0
      ITYPE = 0
      dsel = .FALSE.

      SMDLAM =  1.0D10
      BGDLAM = -1.0D10
      SMDLOM =  1.0D10
      BGDLOM = -1.0D10
      SMDLAS =  1.0D10
      BGDLAS = -1.0D10
      SMDLOS =  1.0D10
      BGDLOS = -1.0D10

      ADLAM = 0.0D0
      VDLAM = 0.0D0
      SDLAM = 0.0D0
      SDLAM2 = 0.0D0
      ADLOM = 0.0D0
      VDLOM = 0.0D0
      SDLOM = 0.0D0
      SDLOM2 = 0.0D0

      ADLAS = 0.0D0
      VDLAS = 0.0D0
      SDLAS = 0.0D0
      SDLAS2 = 0.0D0
      ADLOS = 0.0D0
      VDLOS = 0.0D0
      SDLOS = 0.0D0
      SDLOS2 = 0.0D0

      XSMALL =  1.0D10
      XBIG   = -1.0D10
      YSMALL =  1.0D10
      YBIG   = -1.0D10

      RETURN
      END

*  $Header: /nwiscvs/watstore/geod/src/nadcon/nadcon.f,v 1.1 1998/07/07 17:21:04 grogers Exp $
*  nadcon.f

      PROGRAM NADCON

***********************************************************************
*                                                                     *
* PROGRAM :   NADCON                                                  *
*                                                                     *
* PURPOSE:    COMPUTATION PROGRAM TO CONVERT (OR TRANSFORM)           *
*             POSITIONAL DATA (E.G., LATITUDES AND LONGITUDES) FROM   *
*             THE NORTH AMERICAN DATUM OF 1927 (NAD 27) TO THE        *
*             NORTH AMERICAN DATUM OF 1983 (NAD 83).  THIS PROGRAM    *
*             CAN COMPUTE FROM FROM EITHER DATUM TO THE OTHER.        *
***********************************************************************
*
*  $Log: nadcon.f,v $
*  Revision 1.1  1998/07/07 17:21:04  grogers
*  PR#0, initial add of nadcon
*
*
*             THE ACTUAL COMPUTATION IS PERFORMED AS AN INTERPOLATION *
*             FROM A REGULARLY-SPACED GRID OF POINTS OBTAINED FROM THE*
*             FITTING OF A MINIMUM-CURVATURE SURFACE TO THE ACTUAL    *
*             SHIFT DATA RESULTING FROM THE NAD 83 ADJUSTMENT.        *
*                                                                     *
*             THE INTERPOLATION IS ACCOMPLISHED BY LOCALLY FITTING    *
*             A CURVED POLYNOMIAL SURFACE TO THE FOUR DATA POINTS     *
*             DEFINING THE SQUARE WHICH SURROUND THE (X,Y) PAIR       *
*             WHERE THE INTERPOLATION IS TO TAKE PLACE.               *
*                                                                     *
*             THE POLYNOMIAL SURFACE IS DEFINED BY:                   *
*                                                                     *
*                         A+BX+CY+DXY=Z                               *
*                                                                     *
*             THE PROGRAM REQUIRES THAT THE USER SPECIFY:             *
*                                                                     *
*             1)  THE NAME OF AN OUTPUT FILE                          *
*                                                                     *
*             2)  THE NAME OF AN INPUT FILE (IF AVAILABLE).           *
*                                                                     *
*                                                                     *
*                                                                     *
*             ESTIMATES OF DATUM SHIFTS IN TERMS OF METERS ARE        *
*             COMPUTED FROM THE SHIFT ESTIMATES USING ELLIPSOIDAL     *
*             SCALING.                                                *
*                                                                     *
*             THIS PROGRAM ALLOWS FOR EITHER NGS STANDARD HORIZONTAL  *
*             DATA FORMATS AS SPECIFIED IN THE FGCC PUBLICATION,      *
*             COMMONLY KNOWN AS THE 'HORIZONTAL BLUE BOOK' (SEE       *
*             SUBROUTINE TYPE3), OR IN A GENERIC FILE FORMAT (SEE     *
*             SUBROUTINE TYPE1 OR SUBROUTINE TYPE2).                  *
*                                                                     *
*             THE CODE CAN BE EASILY MODIFIED TO ACCOMMODATE CUSTOM   *
*             FILE SPECIFICATIONS BY MODIFYING SUBROUTINES: ENDREP,   *
*             GETPT, IPARMS, WRTPT, AND (OPTIONALLY) FHELP.           *
*                                                                     *
*                                                                     *
* VERSION CODE:  1.03                                                 *
*                                                                     *
* VERSION DATE:  APRIL 1, 1991                                        *
*                                                                     *
*        AUTHOR:   WARREN T. DEWHURST, PH.D.                          *
*                    LIEUTENANT COMMANDER, NOAA                       *
*                  ALICE R. DREW                                      *
*                    SENIOR GEODESIST, HORIZONTAL NETWORK BRANCH      *
*                  NATIONAL GEODETIC SURVEY, NOS, NOAA                *
*                  ROCKVILLE, MD   20852                              *

c version 2.10 - 1/20/92
c      added option to select HPGN grids and compute NAD 83 - HPGN
c      conversions - jmb
***********************************************************************

***********************************************************************
*                                                                     *
*                  DISCLAIMER                                         *
*                                                                     *
*   THIS PROGRAM AND SUPPORTING INFORMATION IS FURNISHED BY THE       *
* GOVERNMENT OF THE UNITED STATES OF AMERICA, AND IS ACCEPTED AND     *
* USED BY THE RECIPIENT WITH THE UNDERSTANDING THAT THE UNITED STATES *
* GOVERNMENT MAKES NO WARRANTIES, EXPRESS OR IMPLIED, CONCERNING THE  *
* ACCURACY, COMPLETENESS, RELIABILITY, OR SUITABILITY OF THIS         *
* PROGRAM, OF ITS CONSTITUENT PARTS, OR OF ANY SUPPORTING DATA.       *
*                                                                     *
*   THE GOVERNMENT OF THE UNITED STATES OF AMERICA SHALL BE UNDER NO  *
* LIABILITY WHATSOEVER RESULTING FROM ANY USE OF THIS PROGRAM.  THIS  *
* PROGRAM SHOULD NOT BE RELIED UPON AS THE SOLE BASIS FOR SOLVING A   *
* PROBLEM WHOSE INCORRECT SOLUTION COULD RESULT IN INJURY TO PERSON   *
* OR PROPERTY.                                                        *
*                                                                     *
*   THIS PROGRAM IS PROPERTY OF THE GOVERNMENT OF THE UNITED STATES   *
* OF AMERICA.  THEREFORE, THE RECIPIENT FURTHER AGREES NOT TO ASSERT  *
* PROPRIETARY RIGHTS THEREIN AND NOT TO REPRESENT THIS PROGRAM TO     *
* ANYONE AS BEING OTHER THAN A GOVERNMENT PROGRAM.                    *
*                                                                     *
***********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION VRSION
      INTEGER MXAREA
c     PARAMETER (VRSION = 1.03D0, MXAREA = 8)
      PARAMETER (VRSION = 2.10D0, MXAREA = 8)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER KEY, NCONV, IPAGE, ITYPE, IFILE
      LOGICAL PAGE, NODATA, SCREEN,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

**********************
* INITIALIZE VARIABLES
**********************
      CALL INITL (SCREEN, PAGE, IPAGE, ITYPE,
     +            SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +            SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +            ADLAM, VDLAM, SDLAM, SDLAM2,
     +            ADLOM, VDLOM, SDLOM, SDLOM2,
     +            ADLAS, VDLAS, SDLAS, SDLAS2,
     +            ADLOS, VDLOS, SDLOS, SDLOS2,
     +            XSMALL, XBIG, YSMALL, YBIG,dsel)

**************************
* PRINT HEADER INFORMATION
**************************

      CALL HEADR (VRSION)
c
c *********************************
c Print Main Menu; Get Datum Option
c *********************************

    1	CALL menu (dsel)

C *********************************

******************************************************** 
* OPEN NADCON DATA FILES (LATITUDE AND LONGITUDE GRIDS)
*******************************************************

       CALL NGRIDS (NODATA,dsel,VRSION)
       IF (NODATA) GOTO 1

******************************************************
* REQUEST FOR THE NEEDED VARIABLE VALUES FROM THE USER
******************************************************

      CALL IPARMS (KEY, ITYPE, SCREEN,dsel)
*********************************
* LOOP (ONCE FOR EACH CONVERSION)
*********************************

      CALL MLOOP (NCONV, IPAGE, ITYPE, KEY, VRSION,
     +            DLAM, DLOM, DLAS, DLOS,
     +            SDLAM, SDLAM2, SDLOM, SDLOM2,
     +            SDLAS, SDLAS2, SDLOS, SDLOS2,
     +            SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +            SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +            XSMALL, XBIG, YSMALL, YBIG,
     +            PAGE, SCREEN,dsel)

**********************************************
* FINISHED WITH ALL CONVERSIONS - WRITE REPORT
**********************************************

      CALL ENDREP (IPAGE, NCONV, KEY, ITYPE,
     +             DLAM, DLOM, DLAS, DLOS,
     +             ADLAM, ADLOM, VDLAM, VDLOM,
     +             ADLAS, ADLOS, VDLAS, VDLOS,
     +             SDLAM, SDLAM2, SDLOM, SDLOM2,
     +             SDLAS, SDLAS2, SDLOS, SDLOS2,
     +             SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +             SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +             XSMALL, XBIG, YSMALL, YBIG,dsel)

*****************
* CLOSE ALL FILES
*****************

      DO 1010 IFILE = 1, 2*NAREA
        CLOSE (LUAREA(IFILE), STATUS='KEEP')
 1010 CONTINUE
      CLOSE (NIN, STATUS='KEEP')
      CLOSE (NOUT, STATUS='KEEP')
      CLOSE (NAPAR, STATUS='KEEP')

      go to 1

 9999 STOP
      END

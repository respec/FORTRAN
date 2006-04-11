*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/gr_nadcon.f,v 1.1 1998/07/07 19:32:13 grogers Exp $
*  gr_nadcon.f
*
      subroutine gr_nadcon(
     i  option,         !*  (0=open files, 9=close files,
                        !*   1=transform NAD27 to NAD83,
                        !*   2=transform NAD83 to NAD27)
     i  ilat,           !*  input latitude, Sddmmss.sss, char*11
     i  ilon,           !*  input longitude, Sdddmmss.sss, char*12
     o  olat,           !*  output latitude, Sddmmss.sss, char*11
     o  olon,           !*  output longitude, Sdddmmss.sss, char*12
     o  rtncde)         !*  return code (0=no error)
*
* PURPOSE:    COMPUTATION PROGRAM TO CONVERT (OR TRANSFORM)           *
*             POSITIONAL DATA (E.G., LATITUDES AND LONGITUDES) FROM   *
*             THE NORTH AMERICAN DATUM OF 1927 (NAD 27) TO THE        *
*             NORTH AMERICAN DATUM OF 1983 (NAD 83).  THIS PROGRAM    *
*             CAN COMPUTE FROM FROM EITHER DATUM TO THE OTHER.        *
***********************************************************************
*
*  This program adapted from NOAA NADCON.F program
*  See /watstore/geod/src/nadcon/nadcon.f for more information
*
*  $Log: gr_nadcon.f,v $
*  Revision 1.1  1998/07/07 19:32:13  grogers
*  PR#0, initial load of nadcon_lib
*
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      include 'nadcomm.i'
      save
      integer*4 ideg, imin, option, rtncde
      real*8 deg, min, sec
      character ilat*11, ilon*12, olat*11, olon*12

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

      rtncde = 0

*-----------------------------------------------------------------------
*     open files (option = 0)
*-----------------------------------------------------------------------

      if (option .eq. 0)  then
*
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

*GR   CALL HEADR (VRSION)
c
c *********************************
c Print Main Menu; Get Datum Option
c *********************************

*GR 1	CALL menu (dsel)
 1    dsel = .true.

C *********************************

******************************************************** 
* OPEN NADCON DATA FILES (LATITUDE AND LONGITUDE GRIDS)
*******************************************************

       CALL gr_NGRIDS (NODATA,dsel,VRSION)
*GR    IF (NODATA) GOTO 1
       if (nodata)  goto 9900

*-----------------------------------------------------------------------
*     process one point (option =1/2)
*-----------------------------------------------------------------------

      else if (option.eq.1 .or. option.eq.2)  then

******************************************************
* REQUEST FOR THE NEEDED VARIABLE VALUES FROM THE USER
******************************************************

*GR   CALL IPARMS (KEY, ITYPE, SCREEN,dsel)
      if (option .eq. 1)  then
        key = 1                   !*  NAD27 -> NAD83
      else
        key = -1                  !*  NAD83 -> NAD27
      endif

*********************************
* LOOP (ONCE FOR EACH CONVERSION)
*********************************

*
*     save input variables in common block
*
      read (ilat,'(1x,f2.0,f2.0,f6.0)',err=9900) deg, min, sec
      yvalue = deg + min/60. + sec/3600.
      if (ilat(1:1) .eq. '-')  yvalue = -yvalue
      read (ilon,'(1x,f3.0,f2.0,f6.0)',err=9900) deg, min, sec
      xvalue = deg + min/60. + sec/3600.
      if (ilon(1:1) .eq. '-')  xvalue = -xvalue

      CALL gr_MLOOP (NCONV, IPAGE, ITYPE, KEY, VRSION,
     +            DLAM, DLOM, DLAS, DLOS,
     +            SDLAM, SDLAM2, SDLOM, SDLOM2,
     +            SDLAS, SDLAS2, SDLOS, SDLOS2,
     +            SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +            SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +            XSMALL, XBIG, YSMALL, YBIG,
     +            PAGE, SCREEN,dsel)
      if (errcode .ne. 0)  goto 9900
*
*     store output values in common block
*
      call hms(yvalue2, ideg, imin, sec)
      write (olat, '(i3, i2.2, f6.3)') ideg, imin, sec
      if (olat(6:6) .eq. ' ')  olat(6:6) = '0'
      call hms(xvalue2, ideg, imin, sec)
      write (olon, '(i4, i2.2, f6.3)') ideg, imin, sec
      if (olon(7:7) .eq. ' ')  olon(7:7) = '0'

*-----------------------------------------------------------------------
*     close files (option=9)
*-----------------------------------------------------------------------

      else         !*  option must be 9

**********************************************
* FINISHED WITH ALL CONVERSIONS - WRITE REPORT
**********************************************

*GR   CALL ENDREP (IPAGE, NCONV, KEY, ITYPE,
*GR  +             DLAM, DLOM, DLAS, DLOS,
*GR  +             ADLAM, ADLOM, VDLAM, VDLOM,
*GR  +             ADLAS, ADLOS, VDLAS, VDLOS,
*GR  +             SDLAM, SDLAM2, SDLOM, SDLOM2,
*GR  +             SDLAS, SDLAS2, SDLOS, SDLOS2,
*GR  +             SMDLAM, BGDLAM, SMDLOM, BGDLOM,
*GR  +             SMDLAS, BGDLAS, SMDLOS, BGDLOS,
*GR  +             XSMALL, XBIG, YSMALL, YBIG,dsel)

*****************
* CLOSE ALL FILES
*****************

      DO 1010 IFILE = 1, 2*NAREA
        CLOSE (LUAREA(IFILE), STATUS='KEEP')
 1010 CONTINUE
*GR   CLOSE (NIN, STATUS='KEEP')
*GR   CLOSE (NOUT, STATUS='KEEP')
*GR   CLOSE (NAPAR, STATUS='KEEP')

*GR   go to 1

      endif

*GR 9999 STOP

      goto 9999
 9900 rtncde = 1

 9999 return
      END

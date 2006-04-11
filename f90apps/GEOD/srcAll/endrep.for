*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/endrep.f,v 1.1 1998/07/07 19:32:06 grogers Exp $
*  endrep.f
*
      SUBROUTINE ENDREP (IPAGE, NCONV, KEY, ITYPE,
     +                   DLAM, DLOM, DLAS, DLOS,
     +                   ADLAM, ADLOM, VDLAM, VDLOM,
     +                   ADLAS, ADLOS, VDLAS, VDLOS,
     +                   SDLAM, SDLAM2, SDLOM, SDLOM2,
     +                   SDLAS, SDLAS2, SDLOS, SDLOS2,
     +                   SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                   SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                   XSMALL, XBIG, YSMALL, YBIG,dsel)

*  Purpose: Gather statistics and write the end-of-program report
*****************************************************************
*
*  $Log: endrep.f,v $
*  Revision 1.1  1998/07/07 19:32:06  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER IPAGE, NCONV, KEY, ITYPE, LU, I
      LOGICAL PAGE,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      PAGE = .TRUE.
      IPAGE = IPAGE + 1

*******************
* DO THE STATISTICS
*******************

      IF (NCONV .GE. 2) THEN

* calculate mean, variance, standard deviation for both latitude and
* longitude in both meters and seconds of arc.

**********
* LATITUDE
**********

        ADLAM = SDLAM/DBLE(NCONV)
        VDLAM = SDLAM2/DBLE(NCONV-1) - SDLAM**2/DBLE( NCONV*(NCONV-1) )
        ADLAS = SDLAS/DBLE(NCONV)
        VDLAS = SDLAS2/DBLE(NCONV-1) - SDLAS**2/DBLE( NCONV*(NCONV-1) )

        IF (VDLAM .GT. 1.0D-6) THEN
          SDLAM = DSQRT(VDLAM)
        ELSE
          VDLAM = 0.0D0
          SDLAM = 0.0D0
        ENDIF

        IF (VDLAS .GT. 1.0D-6) THEN
          SDLAS = DSQRT(VDLAS)
        ELSE
          VDLAS = 0.0D0
          SDLAS = 0.0D0
        ENDIF

***********
* LONGITUDE
***********

        ADLOM = SDLOM/DBLE(NCONV)
        VDLOM = SDLOM2/DBLE(NCONV-1) - SDLOM**2/DBLE( NCONV*(NCONV-1 ))

        ADLOS = SDLOS/DBLE(NCONV)
        VDLOS = SDLOS2/DBLE(NCONV-1) - SDLOS**2/DBLE( NCONV*(NCONV-1 ))

        IF (VDLOM .GT. 1.0D-6) THEN
          SDLOM = DSQRT(VDLOM)
        ELSE
          VDLOM = 0.0D0
          SDLOM = 0.0
        ENDIF

        IF (VDLOS .GT. 1.0D-6) THEN
          SDLOS = DSQRT(VDLOS)
        ELSE
          VDLOS = 0.0D0
          SDLOS = 0.0
        ENDIF

      ELSEIF (NCONV .LT. 2) THEN
        ADLAM = DLAM
        ADLOM = DLOM
        VDLOM = 0.0D0
        SDLOM = 0.0D0
        VDLAM = 0.0D0
        SDLAM = 0.0D0
        ADLAS = DLAS
        ADLOS = DLOS
        VDLOS = 0.0D0
        SDLOS = 0.0D0
        VDLAS = 0.0D0
        SDLAS = 0.0D0
      ENDIF

***************************************
* PRINT OUT THE STATISTICS FOR THIS JOB
***************************************

      LU = LUOUT
      IF (NCONV .GT. 0) THEN

*************************************************
* FIRST REPORT THE FINAL STATISTICS TO THE SCREEN
*************************************************

        CALL REPORT (LU, SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +               SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +               ADLAM, VDLAM, SDLAM, ADLOM, VDLOM, SDLOM,
     +               ADLAS, VDLAS, SDLAS, ADLOS, VDLOS, SDLOS,
     +               IPAGE, PAGE, KEY,dsel)
        CALL DIAGRM (LU, NCONV, XSMALL, XBIG, YSMALL, YBIG, KEY,
     +  dsel)

****************************************************
* NOW REPORT THE FINAL STATISTICS TO THE OUTPUT FILE
****************************************************

        IF (ITYPE .EQ. 0) THEN

**************************************
* INTERACTIVE USE ONLY - NO INPUT FILE
**************************************

          LU = NOUT
          CALL REPORT (LU, SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                 SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                 ADLAM, VDLAM, SDLAM, ADLOM, VDLOM, SDLOM,
     +                 ADLAS, VDLAS, SDLAS, ADLOS, VDLOS, SDLOS,
     +                 IPAGE, PAGE, KEY,dsel)

          CALL DIAGRM (LU, NCONV, XSMALL, XBIG, YSMALL, YBIG, KEY,
     +    dsel)
          IF (NCONV .EQ. 0) THEN
            DO 1007 I = 1, 2
	     if(dsel) then
              WRITE (LU,*) ' All of your NAD 27 stations are out of',
     +                     ' bounds.'
	     else
	      WRITE(LU,*) ' All of your NAD 83 stations are out of',
     +                    ' bounds.'
	     end if
 1007       CONTINUE
          ENDIF
        ELSEIF (ITYPE .EQ. 1) THEN

* For file format type 1

          LU = NOUT
          CALL REPORT (LU, SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                 SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                 ADLAM, VDLAM, SDLAM, ADLOM, VDLOM, SDLOM,
     +                 ADLAS, VDLAS, SDLAS, ADLOS, VDLOS, SDLOS,
     +                 IPAGE, PAGE, KEY,dsel)

          CALL DIAGRM (LU, NCONV, XSMALL, XBIG, YSMALL, YBIG, KEY,
     +    dsel)

        ELSEIF (ITYPE .EQ. 2) THEN
       
* ITYPE = 2, (free format input, free format output) does not have a
* report written to the output file

        ELSEIF (ITYPE .EQ. 3) THEN
       
* ITYPE = 3, NGS Horizontal Blue Book file format does not have a
* report written to the output file

        ENDIF

      ENDIF

      RETURN
      END

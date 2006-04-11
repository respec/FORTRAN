*  $Header: /nwiscvs/watstore/geod/src/vertcon/vertcon.f,v 1.1 1998/07/07 19:40:56 grogers Exp $
*  vertcon.f
*
      program vertcon
***********************************************************************
*                            Release 2.0                              *
* PURPOSE:    CALCULATE THE DATUM CONVERSION CORRECTION TO NGVD29     *
*             ORTHOMETRIC HEIGHT IN ORDER TO OBTAIN NAVD88 HEIGHT.    *
*             THE INPUT REQUIRES LATITUDE AND LONGITUDE VALUES;       *
***********************************************************************
*
*  $Log: vertcon.f,v $
*  Revision 1.1  1998/07/07 19:40:56  grogers
*  PR#0, initial add of vertcon
*
*
*             [THE GEODETIC DATUM TO WHICH THE POSITION IS REFERENCED *
*             (I.E., NAD27 OR NAD83) IS GENERALLY IRRELEVANT DUE TO   *
*             THE IMPRECISION OF SCALED NAD27 POSITIONS;  HOWEVER,    *
*             SINCE NUMEROUS BENCH MARKS ARE ALSO HORIZONTAL NETWORK  *
*             STATIONS, ALL POSITIONS WILL BE CONVERTED TO NAD83      *
*             POSITIONS IN THE NEXT RELEASE]                          *
*                                                                     *
*             THE COMPUTATION IS PERFORMED AS AN INTERPOLATION USING  *
*             A BILINEAR INTERPOLATOR AMONG FOUR GRID POINTS.         *
*                                                                     *
*             THE PROGRAM REQUIRES THAT THE USER SPECIFY:             *
*             1)  THE NAME OF AN INPUT FILE (IF AVAILABLE).           *
*             2)  THE NAME OF AN OUTPUT FILE                          *
*                                                                     *
*             THIS PROGRAM ALLOWS FOR :                               *
*               GENERIC FILE FORMATS -  SEE SUBROUTINE vTYPE1 OR vTYPE2 *
*                                 OR                                  *
*               NGS INTERNAL BENCH MARK FILE FORMAT - SEE TYPE3       *
*                                                                     *
*            [THE CODE CAN BE EASILY MODIFIED TO ACCOMMODATE CUSTOM   *
*             FILE SPECIFICATIONS (TYPE..) BY MODIFYING SUBROUTINES:  *
*             vGETPT, vIPARMS, vWRTPT, AND (OPTIONALLY) vFHELP.]          *
*                                                                     *
*                                                                     *
* VERSION CODE:  2.00   (August-94)                                   *
*                                                                     *
* INFORMATION CONCERNING THE 'VERTCON' SYSTEM MAY BE OBTAINED FROM:   *
*                  RUDOLF J. FURY  NOAA/NOS/C&GS/NGS                  *
*                  SILVER SPRING, MARYLAND 20910-3282                 *
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
*
c
      real vrsion
      parameter (vrsion = 2.00E0)
      logical page, screen,nogo
      character*60 name
      SAVE NCONV,ITYPE,PAGE,SCREEN
c
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
* INITIALIZE VARIABLES
      CALL vINITL (SCREEN, PAGE, NAME, IPAGE, ITYPE)
* DISCLAIMER
      CALL vHEADR(VRSION)
* LOAD 'VERTCON' GRID
       call loadgrd(nogo)

      if(.not.nogo) then
* REQUEST FOR THE NEEDED VARIABLE VALUES FROM THE USER
      CALL vIPARMS (ITYPE, SCREEN,VRSION)
* LOOP ONCE FOR EACH COMPUTATION
      CALL vMLOOP (NCONV, IPAGE, ITYPE, VRSION,
     +            PAGE, SCREEN, NAME)
      IF(ITYPE.GT.0) THEN 
        CLOSE(NIN,IOSTAT=IOS,ERR=1001,STATUS='KEEP')
        CLOSE(NOUT,IOSTAT=IOS,ERR=1002,STATUS='KEEP')
        call closegrd
        IF(LDUMP.NE.LUOUT)
     +   CLOSE(LDUMP,IOSTAT=IOS,ERR=1003,STATUS='KEEP')
      ENDIF
      endif

      write(LUOUT,*) 'Normal End'
      stop
 1001 write(LUOUT,1104) ios
 1104 format(' Input File i/o err. =',i5)
      stop
 1002 write(LUOUT,1105) ios
 1105 format(' Output File i/o err. =',i5)
      stop
 1003 write(LUOUT,1106) ios
 1106 format(' Dump  File i/o err. =',i5)
      stop
      end                                     

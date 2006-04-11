*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/sgrids.f,v 1.1 1998/07/07 19:32:35 grogers Exp $
*  sgrids.f
*
      SUBROUTINE SGRIDS(VRSION)

* Purpose: SGRIDS is DFILES subroutine revised for state hpgn files
*******************************************************************
*
*  $Log: sgrids.f,v $
*  Revision 1.1  1998/07/07 19:32:35  grogers
*  PR#0, initial load of nadcon_lib
*
*

* This subroutine opens the NADCON grids using the state HPGN
* grid files. Only 1 state hpgn file can be open at any one time
* the program loop will take care of using other states in the same
* run - jmb 1/16/92

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, MXDEF
      PARAMETER (MXAREA = 8, MXDEF = MXAREA)
      CHARACTER*80 B80
      CHARACTER*65 B65
      CHARACTER*20 B20
      CHARACTER*2  state
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
      PARAMETER (B65 = B20//B20//B20//'     ')

      DOUBLE PRECISION XMAX1, XMIN1, YMAX1, YMIN1
      DOUBLE PRECISION DX1, DY1
      INTEGER  ITEMP, NC1
      CHARACTER*80 DUM
      CHARACTER*65 AFILE 
c     CHARACTER*15 DAREAS(MXDEF)
      LOGICAL NOGO, GFLAG

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      DOUBLE PRECISION VRSION
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA DUM / B80 /
c the following does not pertain to state grid files

* DFILES contains the default locations (pathname) of the grid files
* without the .las and .los extensions. (For example 'conus' would
* indicate that the conus.las and conus.los grid files are in the
* current working directory.)  The length of each entry in DFILES may
* be up to 65 characters.  DAREAS contains the default names of these
* areas.  The names are used internally in the program and in the
* program output.  They may be no longer than 15 characters.  They
* must correspond on a one-to-one basis with the file locations in
* the DFILES array.  That is, the first area name in DAREAS must
* be the name that you wish for the first data file set in the
* DFILES array.  You may, of course, have the arrays the same if
* the location of the data file is no longer than 15 characters.
* The locations of the grid files may be differ for each
* installation.  If the pathnames are not correct DFILES (and, possibly,
* DAREAS) may be changed and the program recompiled.


      GFLAG = .FALSE.

c  pick up the state file if no file is in area.par.

        write(LUOUT,90) VRSION
  90    FORMAT(//,' "README.210" file on the program disk contains ',
     + 'the names of the states',/,
     + ' for which you have High Precision grids.',/, 
     + ' Please refer to that file before running NADCON, version ',
     + f5.2,///, ' Enter the two-letter name for the state or state ',
     + 'group you choose')
	  READ(LUIN,95) state
  95       FORMAT(A2)
	  WRITE(LUOUT,'(/)')
	AFILE = B65
	AFILE(1:2) = state
	AFILE(3:6) = 'hpgn'

* Do not print error messages for non-existing files.

        ITEMP = NAREA + 1
        CALL OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX1, DY1,
     +               XMAX1, XMIN1, YMAX1, YMIN1, NC1, DUM)

        IF (.NOT. NOGO) THEN

* Set of files opened OK and variables read

          NAREA = ITEMP
c         AREAS(NAREA) = DAREAS(IDEF)
          DX(NAREA) = DX1
          DY(NAREA) = DY1
          XMAX(NAREA) = XMAX1
          XMIN(NAREA) = XMIN1
          YMAX(NAREA) = YMAX1
          YMIN(NAREA) = YMIN1
          NC(NAREA) = NC1

c         WRITE (LUOUT,120) NAREA, AREAS(NAREA)
c 120     FORMAT (2X, I2, 2X, A15)
	  write(LUOUT,121) NAREA, state
  121     FORMAT (2X,I2,2X,A2)
        ENDIF

  140 CONTINUE

  999 RETURN
      END

*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/dgrids.f,v 1.1 1998/07/07 19:32:04 grogers Exp $
*  dgrids.f
*
      SUBROUTINE DGRIDS

* Purpose: Opens the NADCON grids
*********************************
*
*  $Log: dgrids.f,v $
*  Revision 1.1  1998/07/07 19:32:04  grogers
*  PR#0, initial load of nadcon_lib
*
*
* Purpose: Opens the NADCON grids using the default grid
* names and locations.  The default names of the grid areas are
* given in DAREAS and the default base file locations are in DFILES

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, MXDEF
      PARAMETER (MXAREA = 8, MXDEF = MXAREA)
      CHARACTER*80 B80
      CHARACTER*65 B65
      CHARACTER*20 B20
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
      PARAMETER (B65 = B20//B20//B20//'     ')

      DOUBLE PRECISION XMAX1, XMIN1, YMAX1, YMIN1
      DOUBLE PRECISION DX1, DY1
      INTEGER IDEF, ITEMP, NC1
      CHARACTER*80 DUM
*GR   CHARACTER*65 AFILE, DFILES(MXAREA)
cjk      external ints, nlen$a
cjk      integer*2 ints, nlen$a
cpd      integer*4 len_dir
      character afile*128, dfiles(mxarea)*65
      character data_dir*128

      CHARACTER*15 DAREAS(MXDEF)
      LOGICAL NOGO, GFLAG

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA DUM / B80 /

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

      DATA DFILES /'conus', 'hawaii', 'prvi',
     +             'stlrnc', 'stgeorge', 'stpaul', 'alaska', ' '/
      DATA DAREAS /'Conus', 'Hawaii', 'P.R. and V.I.',
     +             'St. Laurence I.', 'St. George I.', 'St. Paul I.',
     +             'Alaska', ' '/

      GFLAG = .FALSE.
      WRITE (LUOUT, 80)
   80 FORMAT (/, '      Default Data Grids', /,
     +           '   #  AREA NAME', /, 1X, 79('=') )

      data_dir = 'support/geod_grids/'
cjk      call nwf_add_nwishome(data_dir)
cjk      len_dir = nlen$a(data_dir,ints(128))

      DO 140 IDEF = 1, MXDEF
        AFILE = DFILES(IDEF)
        IF (AFILE .EQ. B65) GOTO 999
        afile = data_dir(1:14) // afile

* Try to open a set of default files.
* Do not print error messages for non-existing files.

        ITEMP = NAREA + 1
        CALL OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX1, DY1,
     +               XMAX1, XMIN1, YMAX1, YMIN1, NC1, DUM)

        IF (.NOT. NOGO) THEN

* Set of files opened OK and variables read

          NAREA = ITEMP
          AREAS(NAREA) = DAREAS(IDEF)
          DX(NAREA) = DX1
          DY(NAREA) = DY1
          XMAX(NAREA) = XMAX1
          XMIN(NAREA) = XMIN1
          YMAX(NAREA) = YMAX1
          YMIN(NAREA) = YMIN1
          NC(NAREA) = NC1

          WRITE (LUOUT,120) NAREA, AREAS(NAREA)
  120     FORMAT (2X, I2, 2X, A15)

        ENDIF

  140 CONTINUE

  999 RETURN
      END

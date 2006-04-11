*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/ngrids.f,v 1.1 1998/07/07 19:32:28 grogers Exp $
*  ngrids.f
*
      SUBROUTINE NGRIDS (NODATA,dsel,VRSION)

* Purpose: Opens the NADCON grids which contain datum shifts.
*************************************************************
*
*  $Log: ngrids.f,v $
*  Revision 1.1  1998/07/07 19:32:28  grogers
*  PR#0, initial load of nadcon_lib
*
*
* A total of two files are necessary for each area; 1 for each latitude
* and longitude shift table (gridded data set) expressed in arc seconds.

* If a file named AREA.PAR exists it will be read for the names and
* locations of the gridded data.  The format of the data in
* the AREA.PAR file is given in the GRIDS subroutine.

* If the AREA.PAR file does not exist, or there is still room in the
* arrays in the GDINFO common then the default area names used.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      
      CHARACTER*1 ANS
      LOGICAL NODATA,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      DOUBLE PRECISION VRSION
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* Initialize

      NODATA = .FALSE.
      NAREA = 0
      WRITE (LUOUT,100)
  100 FORMAT (' NADCON is now opening the files containing the',
     +        ' gridded data.', /,
     +        ' The areas listed below may be used for datum',
     +        ' conversions.')

* Try to open the 'AREA.PAR' file in the subroutine GRIDS

      CALL GRIDS(dsel)

* If NAREA>=MXAREA, then skip the section that opens the default files.
* If NAREA<MXAREA or no 'AREA.PAR' file exists, then open default names
* in the subroutine DGRIDS.

c if state hpgn chosen(dsel=false) then only 1 file can be open at 
c a time.  If an hpgn file is not in area.par, then the user can 
c choose a state in SGRIDS.
       if(dsel) then
c default grids chosen
          IF (NAREA .LT. MXAREA) THEN

              CALL DGRIDS

          ENDIF
       else
c state hpgn grids chosen
	  if(NAREA.eq.0) then

	      CALL SGRIDS(VRSION)
	  end if
       end if

      WRITE (LUOUT,975)
  975 FORMAT (/, '  (Hit RETURN to continue.)')
      READ (LUIN,'(A1)') ANS
      WRITE (LUOUT,2)
*   2 FORMAT ('1')
    2 FORMAT ('')

      IF (NAREA .EQ. 0) THEN
        NODATA = .TRUE.
        WRITE (LUOUT, 970)
  970   FORMAT (/, ' ******* ERROR *********', /,
     +          ' No grid files were opened -- program ending!')
      ENDIF

      RETURN
      END

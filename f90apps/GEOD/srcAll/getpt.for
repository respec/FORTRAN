*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/getpt.f,v 1.1 1998/07/07 19:32:10 grogers Exp $
*  getpt.f
*
      SUBROUTINE GETPT (NCONV, ITYPE, KEY, NAME, IDLA, IMLA, SLA,
     +                  IDLO, IMLO, SLO, XPT, YPT,
     +                  EOF, NOPT, FIRST, LAST, IPREC, IFMT,dsel)

* Purpose: Get the name, latitude, and longitude of a point
*          either interactively or from an input data file
***********************************************************
*
*  $Log: getpt.f,v $
*  Revision 1.1  1998/07/07 19:32:10  grogers
*  PR#0, initial load of nadcon_lib
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION XPT, YPT
      DOUBLE PRECISION SLA, SLO
      INTEGER NCONV, ITYPE, KEY, IPREC, IFMT
      INTEGER IDLA, IMLA, IDLO, IMLO
      CHARACTER*80 NAME
      CHARACTER*44 FIRST
      CHARACTER*30 LAST
      CHARACTER*1 ANS
      LOGICAL EOF, NOPT,dsel

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      EOF = .FALSE.
      NOPT = .FALSE.

      IF (ITYPE .EQ. 0) THEN

*************************************
* FOR INTERACTIVE USE - NO INPUT FILE
*************************************

        IF (NCONV .GE. 1) THEN
          WRITE (LUOUT,*) ' Do you want to do another datum',
     +                    ' transformation (Y/N)?'
          WRITE (LUOUT,*) ' (Default is Y)'
          READ (LUIN,'(A1)') ANS
          IF (ANS .EQ. 'n'  .OR.  ANS .EQ. 'N') GOTO 9999
        ENDIF

* Get a point (X,Y) to compute
        CALL ASKPT (NCONV, KEY, NAME, IDLA, IMLA, SLA,
     +              IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT,dsel)
        IF (NOPT) GOTO 9000

      ELSEIF (ITYPE .EQ. 1) THEN

* Free format type 1

        CALL TYPE1 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              XPT, YPT, EOF, NOPT)

      ELSEIF (ITYPE .EQ. 2) THEN

* Free format type 2

        CALL TYPE2 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              XPT, YPT, EOF, NOPT, IFMT)

      ELSEIF (ITYPE .EQ. 3) THEN

* NGS Horizontal Blue Book

        CALL TYPE3 (IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              XPT, YPT, EOF, NOPT, FIRST, LAST, IPREC)

      ENDIF

*********************************************************
* CHANGE THE LONGITUDE TO POSITIVE EAST FOR INTERPOLATION
*********************************************************

      XPT = -XPT

 9000 RETURN

* End of file

 9999 CONTINUE
      EOF = .TRUE.
      GOTO 9000
      END

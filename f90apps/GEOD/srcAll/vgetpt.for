*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vgetpt.f,v 1.1 1998/07/07 20:10:34 grogers Exp $
*  vgetpt.f
*
      SUBROUTINE vGETPT (NCONV, VRSION,ITYPE, NAME, IDLA, IMLA, SLA, 
     +      IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
*
* Purpose: Get the name, latitude, and longitude of a point
* either interactively or from an input data file
***********************************************************
*
*  $Log: vgetpt.f,v $
*  Revision 1.1  1998/07/07 20:10:34  grogers
*  PR#0, initial add of vertcon_lib
*
*
      DOUBLE PRECISION XPT, YPT
      REAL SLA, SLO
      INTEGER NCONV, ITYPE
      INTEGER IDLA, IMLA, IDLO, IMLO
      CHARACTER*40 NAME
      CHARACTER*1 ANS
      LOGICAL EOF, NOPT
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
      EOF = .FALSE.
      NOPT = .FALSE.
      IF (ITYPE .EQ. 0) THEN
* FOR INTERACTIVE USE - NO INPUT FILE
        IF (NCONV .GE. 1) THEN
          WRITE (LUOUT,*) ' Do you want to do another',
     +                    ' computation (Y/N)?'
          WRITE (LUOUT,*) ' (Default is Y)'
          READ (LUIN,'(A1)') ANS
          IF (ANS .EQ. 'n'  .OR.  ANS .EQ. 'N') GOTO 9999
        ENDIF
* Get a point (X,Y) to compute
        CALL vASKPT (NCONV, NAME, IDLA, IMLA, SLA,
     +              IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
        IF (NOPT) GOTO 9000
      ELSEIF (ITYPE .EQ. 1) THEN
* Free format type 1
        CALL vTYPE1 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              XPT, YPT, EOF, NOPT)
         IF(NOPT) STOP
      ELSEIF (ITYPE .EQ. 2) THEN
* Free format type 2
        CALL vTYPE2 (NAME, IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              XPT, YPT, EOF, NOPT)
         IF(NOPT) STOP
      ELSEIF ((ITYPE .EQ. 3).OR.(ITYPE.EQ.4)) THEN
* Files Format type 3 or 4
        CALL TYPE34 (NCONV,VRSION,ITYPE, NAME, IDLA, IMLA, SLA, 
     +     IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
         IF(NOPT) STOP
      ENDIF
 9000 RETURN
* End of file
 9999 EOF = .TRUE.
      RETURN
      END

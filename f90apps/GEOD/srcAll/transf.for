*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/transf.f,v 1.1 1998/07/07 19:32:38 grogers Exp $
*  transf.f
*
      SUBROUTINE TRANSF (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +                   DLAM, DLOM, DLAS, DLOS, KEY, ITYPE)

* Purpose: Computes either the forward or inverse coordinate
* transformation depending upon the value of the integer variable 'key'
***********************************************************************
*
*  $Log: transf.f,v $
*  Revision 1.1  1998/07/07 19:32:38  grogers
*  PR#0, initial load of nadcon_lib
*
*
c 1/20/92 - IF the HPGN option is chosen, statements in this subroutine
c which refer to NAD 27 apply to NAD 83; 
c statements which refer to NAD 83 apply to HPGN -jmb

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, ITMAX
      DOUBLE PRECISION SMALL
      PARAMETER (MXAREA = 8, ITMAX = 10, SMALL = 1.0D-9 )

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      DOUBLE PRECISION XTEMP, YTEMP, XDIF, YDIF
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION DXLAST, DYLAST
      INTEGER KEY, NUM, ITYPE
      CHARACTER*15 RESP
      LOGICAL NOGO

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      IF (KEY .EQ. 1) THEN

**********************
* FOR NAD 27 TO NAD 83
**********************

        CALL TO83 (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +             DLAM, DLOM, DLAS, DLOS, ITYPE)

      ELSEIF (KEY .EQ. -1) THEN

***************************
* FOR NAD 83 TO NAD 27)
* THIS IS DONE BY ITERATION
***************************

        NUM = 0

**************************************************
* SET THE XPT,YPT TO TEMPORARY VALUES
* (REMEMBER, XPT AND YPT ARE REALLY NAD 83 VALUES)
**************************************************

        XTEMP = XPT
        YTEMP = YPT

**************************************************************
* PRETEND THAT THESE TEMPORARY VALUES ARE REALLY NAD 27 VALUES
* FOR A FIRST GUESS AND COMPUTE PSEUDO-NAD 83 COORDINATES
**************************************************************

  200   CONTINUE
          NUM = NUM + 1

********************************
* CHECK THE NUMBER OF ITERATIONS
********************************

          IF (NUM .GE. ITMAX) THEN
            WRITE (LUOUT,*) ' *** MAXIMUM ITERATIONS EXCEEDED!! ***'
            WRITE (LUOUT,*) ' *** CALL PROGRAMMER FOR HELP ***'
            WRITE (LUOUT,*) ' LATITUDE =', YTEMP, ' LONGITUDE =', XTEMP
            WRITE (LUOUT,*) ' GRID AREA =', RESP
            NOGO = .TRUE.
            GOTO 1000
          ENDIF

          CALL TO83 (NOGO, RESP, XTEMP, YTEMP, XPT2, YPT2,
     +               DLAM, DLOM, DLAS, DLOS, ITYPE)
          DXLAST = DLOS
          DYLAST = DLAS

**************************************
* COMPARE TO ACTUAL NAD 83 COORDINATES
**************************************

          XDIF = XPT - XPT2
          YDIF = YPT - YPT2

****************************************************************
* COMPUTE A NEW GUESS UNLESS THE DIFFERENCES ARE LESS THAN SMALL
* WHERE SMALL IS DEFINED (ABOVE) TO BE;  SMALL = 1.0D-9
****************************************************************

          IF (NUM .EQ. 1) THEN
            IF (DABS(XDIF) .GT. SMALL) THEN
              XTEMP = XPT - DLOS/3600.D0
            ENDIF
            IF (DABS(YDIF) .GT. SMALL) THEN
              YTEMP = YPT - DLAS/3600.D0
            ENDIF
          ELSE
            IF (DABS(XDIF) .GT. SMALL) THEN
              XTEMP = XTEMP - (XPT2 - XPT)
            ENDIF
            IF (DABS(YDIF) .GT. SMALL) THEN
              YTEMP = YTEMP - (YPT2 - YPT)
            ENDIF

          ENDIF

          IF (DABS(YDIF) .LE. SMALL  .AND.  DABS(XDIF) .LE. SMALL) THEN

******************************
* IF CONVERGED THEN LEAVE LOOP
******************************

            XPT = XTEMP
            YPT = YTEMP
            GOTO 1000
          ENDIF

*******************************
* IF NOT CONVERGED THEN ITERATE
*******************************

        GOTO 200

      ENDIF
 1000 RETURN
      END

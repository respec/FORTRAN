C
C
C
      SUBROUTINE   PROPLT
     I                    (MESSFL,ICHK,BWNNAM,
     O                     WNDFLG)
C
C     + + + PURPOSE + + +
C     Perform interactive modifications to plot specifications
C     such as device type, axes, titles, curves, plot size,
C     legends, axis scales, extra text as determined by ICHK array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,ICHK(7),WNDFLG
      CHARACTER*8  BWNNAM(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     ICHK   - flag (0=needs input, 1= ok to change, 2= no change)
C              1 - for graphics device
C              2 - for axes types
C              3 - for titles
C              4 - for specs on curves and legends
C              5 - for axes scales
C              6 - for extra text, blank space, and letter style
C              7 - size of plot and characters
C     BWNNAM - window name and path
C              (1) window name, usually Modify
C              (2) path, usually GM
C     WNDFLG - flag for change in device type or device code
C              0 - no change
C              1 - changed, need to close and reopen workstation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I0,I1,SCLU,SGRP,IPAR,INIT,LEN,LPTH,
     &             ILOC(7), WNDW1, WNDW2
      CHARACTER*1  BLNK,PBUFF(49)
      CHARACTER*7  PATHS(8)
      CHARACTER*8  PTHNAM(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, PRNTXT, PMXCNW, ZBLDWR
      EXTERNAL     GETTXT, CARVAR, ZWNSOP, PINAXT, PINSPC, ZSTCMA
      EXTERNAL     PINEXA, PINSCL, PINDEV, PINTTL, PINSIZ
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      BLNK= ' '
      PTHNAM(2) = BWNNAM(2)
      SCLU= 32
C
C     get names of parameter groups
      SGRP= 2
      LEN = 49
      CALL GETTXT (MESSFL,SCLU,SGRP,LEN,PBUFF)
      LEN = 7
      J   = 1
      DO 5 I = 1, 7
        CALL CARVAR ( LEN, PBUFF(J), LEN, PATHS(I) )
        J = J + 7
 5    CONTINUE
      PATHS(8) = 'Return'
C
C     see if any specs need defining
      IPAR= 0
      I   = 0
 10   CONTINUE
        I = I + 1
        IF (ICHK(I).EQ.0) THEN
C         needs defining, set as default for menu
          IPAR = I
        END IF
      IF (I.LT.7 .AND. IPAR.EQ.0) GO TO 10
      IF (IPAR.EQ.0) THEN
C       no undefined parts, default to first part which may be modified
        I= 0
 20     CONTINUE
          I= I+ 1
          IF (ICHK(I).EQ.1) THEN
C           this one is ok to modify, make default for menu
            IPAR= I
          END IF
        IF (I.LT.7 .AND. IPAR.EQ.0) GO TO 20
      END IF
C
C     Initialize local variable with ICHK
      DO 30 I = 1,7
        ILOC(I) = ICHK(I)
 30   CONTINUE
C
      WNDFLG = 0
 40   CONTINUE
C       Modify: 1-Device, 2-Axes, 3-Titles, 4-Curves, 5-Min/Max,
C               6-Extra,  7-Size, 8-Return
        LPTH = 2
        CALL ZWNSOP ( LPTH, BWNNAM )
        SGRP = 1
        CALL QRESP (MESSFL,SCLU,SGRP,IPAR)
        IF (IPAR.LT.8) THEN
C         wants to modify something
          PTHNAM(1) = PATHS(IPAR)
          PTHNAM(3) = PATHS(IPAR)(1:1)
          IF (ICHK(IPAR) .EQ. 2) THEN
C           option not available to user
            LPTH = 3
            CALL ZWNSOP ( LPTH, PTHNAM )
            SGRP = 3
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           ok to modify, allow previous command
            I= 4
            CALL ZSTCMA (I,I1)
            GO TO (100,200,300,400,500,600,700) IPAR
C
 100        CONTINUE
C             device type?
              CALL PINDEV (MESSFL,SCLU,PTHNAM(2), WNDW1)
              IF (WNDW1 .EQ. 1) WNDFLG = 1
              ILOC(1) = 1
              GO TO 800
C
 200        CONTINUE
C             axes types
              CALL PINAXT (MESSFL,SCLU,PTHNAM(2))
              ILOC(2) = 1
              GO TO 800
C
 300        CONTINUE
C             titles
              CALL PINTTL (MESSFL,SCLU,PTHNAM(2))
              ILOC(3) = 1
              GO TO 800
C
 400        CONTINUE
C             specs on the curves
              CALL PINSPC (MESSFL,SCLU,PTHNAM(2))
              ILOC(4) = 1
              GO TO 800
C
 500        CONTINUE
C             scales
              CALL PINSCL (MESSFL,SCLU,PTHNAM(2))
              ILOC(5) = 1
              GO TO 800
C
 600        CONTINUE
C             extra conditions
              CALL PINEXA (MESSFL,SCLU,PTHNAM(2))
              ILOC(6) = 1
              GO TO 800
C
 700        CONTINUE
C             sizes of plot
              CALL PINSIZ (MESSFL,SCLU,PTHNAM(2), WNDW2)
              IF (WNDW2 .EQ. 1) WNDFLG = 1
              ILOC(7) = 1
              GO TO 800
C
 800        CONTINUE
          END IF
        ELSE
C         user wants out, check for any specs which need defining
          INIT= 1
          I   = 0
 900      CONTINUE
            I= I+ 1
            IF (ILOC(I).EQ.0) THEN
C             this part still undefined
              IF (INIT.EQ.1) THEN
C               first undefined group, write header
                LPTH = 3
                PTHNAM(1) = PATHS(8)
                PTHNAM(3) = PATHS(8)(1:1)
                CALL ZWNSOP ( LPTH, PTHNAM )
                SGRP= 4
                CALL PMXCNW (MESSFL,SCLU,SGRP,I1,INIT,I1,J)
                INIT= -1
                J= I
              END IF
              LEN = 7
              CALL ZBLDWR (LEN,PATHS(I),I0,-I1,J)
            END IF
          IF (I.LT.7) GO TO 900
          IF (INIT.EQ.-1) THEN
C           messages written, hold for user
C           turn off previous command
            I= 4
            CALL ZSTCMA (I,I0)
            CALL ZBLDWR (I0,BLNK,I0,I0,J)
C           set IPAR for next screen to do
            IPAR= J
          END IF
        END IF
C
C       turn off previous command
        I= 4
        CALL ZSTCMA (I,I0)
C
      IF (IPAR.NE.8) GO TO 40
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINDEV
     I                   (MESSFL,SCLU,WNDNAM,
     O                    WNDW)
C
C     + + + PURPOSE + + +
C     This routine is called from PROPLT and will allow the user to
C     select graphics output device.   For DG AVIION, a file name
C     will be requested for all devices but the screen (X window).
C
C     + + + HISTORY + + + 
C     kmf - Oct 24, 2000, force old output file to be closed when
C                         a new file is opened.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WNDW
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - keystrokes
C     WNDW   - flag for change in device type or device code
C              0 - no change
C              1 - changed, need to close and reopen workstatio
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP1(5), SGRP, OLDCOD, OLDTYP, IND,
     $            INDEX(4), CPU, IRET, RETCOD, LPTH
      CHARACTER*8 PTH(3), PTHNAM(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESP, ANPRGT, ZWNSOP, ZGTRET, QFOPEN
C
C     + + + DATA INITIALIZATIONS + + +
C                    pc prime vax  unix aviion
      DATA  SGRP1 / 142, 143, 144, 145, 146 /
C                   screen laser    plotter    meta
      DATA  INDEX /   40,    41,       42,      43   /
      DATA   PTH  /       'Laser', 'Plotter', 'Meta' /
C
C     + + + END SPECIFICATIONS + + +
C
C     find computer type (1-pc,2-prime,3-vax,4-unix,5-aviion)
      IND = 1
      CALL ANPRGT (IND,CPU)
      IF (CPU .LT. 1  .OR.  CPU .GT. 5) THEN
C       unknown cpu type, default to pc
        CPU = 1
      END IF
C
      OLDCOD = DEVCOD
      OLDTYP = DEVTYP
 100  CONTINUE
C       DEVTYP: pc: 142: 1-Screen, 2-Laser, 3-Plotter, 4-Meta
C            prime: 143: 1-Screen, 2-Laser, 3-Plotter, 4-Meta/DIS
C              vax: 144: 1-Screen, 2-Laser, 3-Plotter, 4-Meta/gks
C             unix: 145: 1-Screen, 2-Laser, 3-Plotter, 4-Meta/cgm
C           aviion: 146: 1-Screen, 2-Laser, 3-Plotter, 4-Meta/cgm
        LPTH = 1
        CALL ZWNSOP ( LPTH, WNDNAM )
        SGRP = SGRP1(CPU)
        CALL QRESP (MESSFL,SCLU,SGRP,DEVTYP)
        IF (DEVTYP .LT. 1  .OR.  DEVTYP .GT. 4) THEN
C         unknown device type, default to screen
          DEVTYP = 1
        END IF
C       get device code
        CALL ANPRGT ( INDEX(DEVTYP), DEVCOD )
        IF (CPU.EQ.5 .AND. DEVTYP.GT.1) THEN
C         need a file name for graphics meta output for Aviion
          LPTH = 3
          PTHNAM(2) = WNDNAM(1)
          PTHNAM(1) = PTH(1)
          PTHNAM(3) = PTH(1)(1:1)
          CALL ZWNSOP ( LPTH, PTHNAM )
          SGRP = 149
          CALL QFOPEN (MESSFL, SCLU, SGRP, MTPLUT, RETCOD)
          CALL ZGTRET (IRET)
C         force old file to be closed, kmf Oct 24, 2000
          WNDW = 1
        ELSE
C         no other action required
          IRET = 1
        END IF
C       if "Prev"ious, go back to menu for device type
      IF (IRET .EQ. 2) GO TO 100
      IF (OLDCOD .NE. DEVCOD  .OR.  OLDTYP .NE. DEVTYP) THEN
C       new device for output
        WNDW = 1
      ELSE
C       no change
        WNDW = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINAXT
     I                   (MESSFL,SCLU,WNDNAM)
C
C     + + + PURPOSE + + +
C     This routine is called from PROPLT and will allow the user to
C     set the specifications for the types of axes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - keystrokes
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP,IDUM(3),IRET,IC,LPTH,AGAIN,I,RESCLE(3),I1
      REAL        MMIN(3), MMAX(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZWNSOP, Q1INIT, QSETCO, Q1EDIT, QGETCO, PRNTXT
      EXTERNAL    SCALIT
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
      IF (XTYPE.EQ.0) THEN
C       time-series plots
 100    CONTINUE
C         Type for axes: left y-axis, right y-axis, aux axis (1-n,2-y)
          IDUM(1)= YTYPE(1)
          IF (YTYPE(2).GT.0) THEN
            IDUM(2)= YTYPE(2)+ 1
          ELSE
            IDUM(2)= 1
          END IF
          IF (ALEN.GT.0.00001) THEN
            IDUM(3)= 2
          ELSE
            IDUM(3) = 1
          END IF
          IC = 3
          SGRP= 10
          LPTH = 1
          CALL ZWNSOP ( LPTH, WNDNAM )
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
          CALL QSETCO ( IC, IDUM )
          CALL Q1EDIT ( IRET )
          IF (IRET .EQ. 1) THEN
C           process input
            CALL QGETCO ( IC, IDUM )
C
            RESCLE(1) = 0
            RESCLE(2) = 0
            RESCLE(3) = 0
            IF (YTYPE(1) .NE. IDUM(1)    .OR.
     $          YTYPE(2) .NE. IDUM(2)-1) THEN
C             type of axis has changed
              DO 110 I = 1,3
C               initialize limits
                MMIN(I) = 1.0E10
                MMAX(I) = -1.0E10
 110          CONTINUE
C
              DO 120 I = 1,NVAR    
C               set transformation array
                IF (WHICH(I) .EQ. 1) THEN
C                 left y axis
                  TRANSF(I) = IDUM(1)
                  IF (YMAX(I) .GT. MMAX(1)) MMAX(1) = YMAX(I)
                  IF (YMIN(I) .LT. MMIN(1)) MMIN(1) = YMIN(I)
                  RESCLE(1) = 1
                ELSE IF (WHICH(I) .EQ. 2) THEN
C                 right y axis
                  TRANSF(I) = IDUM(2) - 1
                  IF (YMAX(I) .GT. MMAX(2)) MMAX(2) = YMAX(I)
                  IF (YMIN(I) .LT. MMIN(2)) MMIN(2) = YMIN(I)
                  RESCLE(2) = 1
                ELSE IF (WHICH(I) .EQ. 3) THEN
C                 aux axis
                  TRANSF(I) = 1
                  IF (YMAX(I) .GT. MMAX(3)) MMAX(3) = YMAX(I)
                  IF (YMIN(I) .LT. MMIN(3)) MMIN(3) = YMIN(I)
                  RESCLE(3) = 1
                ELSE
C                 shouldn't be an option, set ot 0
                  TRANSF(I) = 0
                END IF
 120          CONTINUE
            END IF
C
            YTYPE(1)= IDUM(1)
            YTYPE(2)= IDUM(2) - 1
            IF (RESCLE(1) .EQ. 1) THEN
              CALL SCALIT (YTYPE(1),MMIN(1),MMAX(1),PLMN(1),PLMX(1))
              IF (YTYPE(1) .EQ. 1) TICS(1) = 10
            END IF
            IF (RESCLE(2) .EQ. 1) THEN
              CALL SCALIT (YTYPE(2),MMIN(2),MMAX(2),PLMN(2),PLMX(2))
              IF (YTYPE(2) .EQ. 1) TICS(2) = 10
            END IF
            IF (RESCLE(3) .EQ. 1) THEN
              CALL SCALIT (I1,      MMIN(3),MMAX(3),PLMN(3),PLMX(3))
              TICS(3) = 2
            END IF
            IF (IDUM(3).EQ.2) THEN
              ALEN = 1.0
            ELSE
              ALEN = 0.0
            END IF
            AGAIN = 0
          ELSE IF (IRET .EQ. -1) THEN
C           "Oops" try again
            AGAIN = 1
          ELSE
C           assume "Prev" back to modify
            AGAIN = 0
          END IF
        IF (AGAIN .EQ. 1) GO TO 100
C
        IF ((YTYPE(2) .GT. 0 .AND. RESCLE(2) .EQ. 0) .OR.
     $      (ALEN .GT. 0.0001 .AND. RESCLE(3) .EQ. 0)) THEN
C         warning to select curves to put curve on right y-axis
          SGRP = 15
          LPTH = 1
          CALL ZWNSOP ( LPTH, WNDNAM )
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
C
      ELSE
C       X-Y plot
 200    CONTINUE
C         type for x and y axes
          IDUM(1)= XTYPE
          IDUM(2)= YTYPE(1)
          IDUM(3)= YTYPE(2) + 1
          IC = 3
          SGRP = 11
          LPTH = 1
          CALL ZWNSOP ( LPTH, WNDNAM )
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
          CALL QSETCO ( IC, IDUM )
          CALL Q1EDIT ( IRET )
          IF (IRET .EQ. 1) THEN
C           process input
            CALL QGETCO ( IC, IDUM )
C
            RESCLE(1) = 0
            RESCLE(2) = 0
            RESCLE(3) = 0
            IF (XTYPE .NE. IDUM(1)       .OR.
     $          YTYPE(1) .NE. IDUM(2)    .OR.
     $          YTYPE(2) .NE. IDUM(3)-1) THEN
C             type of axis has changed
              DO 210 I = 1,3
C               initialize limits
                MMIN(I) = 1.0E10
                MMAX(I) = -1.0E10
 210          CONTINUE
C
              DO 220 I = 1,NVAR
C               set transformation array
                IF (WHICH(I) .EQ. 1) THEN
C                 left y axis
                  TRANSF(I) = IDUM(2)
                  IF (YMAX(I) .GT. MMAX(1)) MMAX(1) = YMAX(I)
                  IF (YMIN(I) .LT. MMIN(1)) MMIN(1) = YMIN(I)
                  RESCLE(1) = 1
                ELSE IF (WHICH(I) .EQ. 2) THEN
C                 right y axis
                  TRANSF(I) = IDUM(3) - 1
                  IF (YMAX(I) .GT. MMAX(2)) MMAX(2) = YMAX(I)
                  IF (YMIN(I) .LT. MMIN(2)) MMIN(2) = YMIN(I)
                  RESCLE(2) = 1
                ELSE IF (WHICH(I) .EQ. 4) THEN
C                 x axis
                  TRANSF(I) = IDUM(1)
                  IF (YMAX(I) .GT. MMAX(3)) MMAX(3) = YMAX(I)
                  IF (YMIN(I) .LT. MMIN(3)) MMIN(3) = YMIN(I)
                  RESCLE(3) = 1
                ELSE
C                 shouldn't be an option, set ot 0
                  TRANSF(I) = 0
                END IF
 220          CONTINUE
            END IF
C
            XTYPE = IDUM(1)
            YTYPE(1) = IDUM(2)
            YTYPE(2) = IDUM(3) - 1
            IF (RESCLE(1) .EQ. 1) THEN
              CALL SCALIT (YTYPE(1),MMIN(1),MMAX(1),PLMN(1),PLMX(1))
              IF (YTYPE(1) .EQ. 1) TICS(1) = 10
            END IF
            IF (RESCLE(2) .EQ. 1) THEN
              CALL SCALIT (YTYPE(2),MMIN(2),MMAX(2),PLMN(2),PLMX(2))
              IF (YTYPE(2) .EQ. 1) TICS(2) = 10
            END IF
            IF (RESCLE(3) .EQ. 1) THEN
              CALL SCALIT (XTYPE,MMIN(3),MMAX(3),PLMN(4),PLMX(4))
              IF (XTYPE .EQ. 1) TICS(4) = 10
            END IF
            ALEN = 0.0
            AGAIN = 0
          ELSE IF (IRET .EQ. -1) THEN
C           "Oops" try again
            AGAIN = 1
          ELSE
C           assume "Prev" back to modify
            AGAIN = 0
          END IF
        IF (AGAIN .EQ. 1) GO TO 200
C
        IF (YTYPE(2) .GT. 0 .AND. RESCLE(2) .EQ. 0) THEN
C         warning to select curves to put curve on right y-axis
          SGRP = 16
          LPTH = 1
          CALL ZWNSOP ( LPTH, WNDNAM )
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
       END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINTTL
     I                   ( MESSFL, SCLU, WNDNAM )
C
C     + + + PURPOSE + + +
C     Allow user to set or modify the plot and axis titles.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     SCLU   - cluster number on message file
C     WNDNAM - keystrokes
C
C     + + + PARAMETERS + + +
      INTEGER  OPTS, TTLS
      PARAMETER ( OPTS=6, TTLS=5 )
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP(OPTS), NFLD(OPTS), LENT(OPTS), LFLD(11,OPTS), 
     $            BLEN(TTLS), BROW(TTLS), BLOC(TTLS,OPTS), BLNT(TTLS),
     $            OPT, RTCMND, LPTH
      CHARACTER*1 TITLES(468), BLNK
C
C     + + + LOCAL DEFINITIONS + + +
C     OPT    - option for titles
C              1 - plot title and left y-axis
C              2 - plot title, left y-axis and aux axis
C              3 - plot title, left and right y-axis, and aux axis
C              4 - plot title and left and right y-axis
C              5 - plot title, left y-axis, and x-axis
C              6 - plot title, left and right y-axis, and x-axis
C     NFLD   - number of fields for each option
C     LENT   - total length of titles required for each option
C     LFLD   - length of each field for each option
C     BLEN   - length of each row of a title or axis
C     BROW   - number of rows for each title or axis
C     BLOC   - location of the title in the screen buffer
C              0 indicates that title not included for the option
C
C     + + + EXTERNALS + + +
      EXTERNAL    PLTITI, PLTITO, OPTNAX
      EXTERNAL    ZIPC
      EXTERNAL    Q1INIT, QSETCT, Q1EDIT, QGETCT, ZWNSOP
C
C     + + + DATA INITIALIZATIONS + + +
C
      DATA  SGRP / 101, 102, 103, 104, 105, 106 /
      DATA  NFLD /   5,   9,  11,   7,   7,   9 /
      DATA  LENT / 312, 360, 438, 390, 390, 468 /
      DATA  LFLD / 3*78, 2*39, 6*0,
     $             3*78, 2*39, 4*12, 2*0,
     $             3*78, 2*39, 2*39, 4*12,
     $             3*78, 2*39, 2*39, 4*0,
     $             3*78, 2*39, 2*39, 4*0,
     $             3*78, 2*39, 2*39, 2*39, 2*0 /
C                 title, left, aux, right, x
      DATA  BLEN /  78,    39,  12,   39,  39 /
      DATA  BROW /   3,     2,   4,    2,   2 /
      DATA  BLNT / 240,    80,  80,   80,  80 /
      DATA  BLOC /   1,   235,   0,    0,   0,
     $               1,   235, 313,    0,   0,
     $               1,   235, 391,  313,   0,
     $               1,   235,   0,  313,   0,
     $               1,   235,   0,    0, 313,
     $               1,   235,   0,  313, 391 /
C
C     + + + END SPECIFICATIONS + + +
C
C     get option for axis to determine required titles
      CALL OPTNAX ( XTYPE, YTYPE, ALEN, OPT )
      IF (OPT .LT. 1  .OR.  OPT .GT. 6) THEN
C       unknown option, default to most fields
        OPT = 3
      END IF
C     clean out titles buffer
      BLNK = ' '
      CALL ZIPC ( LENT(5), BLNK, TITLES )
C
 100  CONTINUE
C       stuff current titles into titles buffer, as appropriate
C       always include plot title and left axis title
        CALL PLTITI ( BLOC(1,OPT), BROW(1), BLEN(1), BLNT(1), TITL,
     M                TITLES )
        CALL PLTITI ( BLOC(2,OPT), BROW(2), BLEN(2), BLNT(2), YLABL,
     M                TITLES )
C       include other titles as appropriate for the option
        IF (BLOC(3,OPT) .NE. 0) THEN
C         auxilary axis included
          CALL PLTITI ( BLOC(3,OPT), BROW(3), BLEN(3), BLNT(3), YALABL,
     M                  TITLES )
        END IF
        IF (BLOC(4,OPT) .NE. 0) THEN
C         right axis included
          CALL PLTITI ( BLOC(4,OPT), BROW(4), BLEN(4), BLNT(4), YXLABL,
     M                  TITLES )
        END IF
        IF (BLOC(5,OPT) .NE. 0) THEN
C         x-axis include
          CALL PLTITI ( BLOC(5,OPT), BROW(5), BLEN(5), BLNT(5), XLABL,
     M                  TITLES )
        END IF
C
C       initialize the question and fill in titles
        LPTH = 1
        CALL ZWNSOP ( LPTH, WNDNAM )
        CALL Q1INIT ( MESSFL, SCLU, SGRP(OPT) )
        CALL QSETCT ( NFLD(OPT), LFLD(1,OPT), LENT(OPT), TITLES )
C
C       edit the screen
        CALL Q1EDIT ( RTCMND )
C       re-set titles if user OOPSed
      IF (RTCMND .EQ. -1) GO TO 100
C
      IF (RTCMND .EQ. 1) THEN
C       user "Accept"ed screen, get values
        CALL QGETCT ( NFLD(OPT), LFLD(1,OPT), LENT(OPT), TITLES )
C       always include plot title and left axis title
        CALL PLTITO ( BLOC(1,OPT), BROW(1), BLEN(1), BLNT(1), TITLES,
     O                TITL )
        CALL PLTITO ( BLOC(2,OPT), BROW(2), BLEN(2), BLNT(2), TITLES,
     O                YLABL )
C       retrieve other titles as appropriate for the option
        IF (BLOC(3,OPT) .NE. 0) THEN
C         auxilary axis included
          CALL PLTITO ( BLOC(3,OPT), BROW(3), BLEN(3), BLNT(3), TITLES,
     O                  YALABL )
        END IF
        IF (BLOC(4,OPT) .NE. 0) THEN
C         right axis included
          CALL PLTITO ( BLOC(4,OPT), BROW(4), BLEN(4), BLNT(4), TITLES,
     O                  YXLABL )
        END IF
        IF (BLOC(5,OPT) .NE. 0) THEN
C         x-axis include
          CALL PLTITO ( BLOC(5,OPT), BROW(5), BLEN(5), BLNT(5), TITLES,
     O                  XLABL )
        ENDIF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINSPC
     I                   (MESSFL,SCLU,WNDNAM)
C
C     + + + PURPOSE + + +
C     This routine is called from PROPLT and will allow the user to
C     input or modify the specifications for each curve including
C     line type, symbol, pattern, color, and labels for the legend.
C     Mean or point is included if the plot is a time-series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 WNDNAM(3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - window name
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   PINCTM, PINCXY
C
C     + + + END SPECIFICATIONS + + +
C
      IF (XTYPE .EQ. 0) THEN
C       time series plot
        CALL PINCTM ( MESSFL, SCLU, WNDNAM )
      ELSE 
C       x-y plot
        CALL PINCXY ( MESSFL, SCLU, WNDNAM )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINCTM
     I                    ( MESSFL, SCLU, WNDNAM )
C
C     + + + PURPOSE + + +
C     Allows user to input or modify the specifications for each curve
C     in a time series plot, including line type, symbol, pattern, color,
C     mean or point, and labels for the legend.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - KEYSTROKES
C
C     + + + PARAMETERS
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP, AGAIN, INIT, IWRT, IRET, MAXL, SCNFG,
     $            FLGVT, FLGLS, FLGAY, FLGAN, FLGRY, FLGRN,
     $            I, J, K, M, L, L1, L20, L80, IC, LPTH, LD(1,TSMAX),
     $            IDUM(7,3,TSMAX), RESP, TYPE, SCLE
      REAL        R(1,TSMAX)
      CHARACTER*1 BLNK, BUFF(80,TSMAX), OV(20)
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCALEM
      EXTERNAL   Q1INIT, QSETCO, Q1EDIT, QGETCO, QRESCX, QRESP
      EXTERNAL   ZWNSOP, ZMNSST, ZGTRET, PMXCNW, CHRCHR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L1, L20, L80, MAXL, SCNFG, BLNK
     $    /  1,  20,  80,   10,     1, ' ' /
      DATA  OV /'O','n','l','y',' ','v','a','r','i','a','b','l','e',
     $         ' ',' ',' ',' ',' ',' ',' '/
C
C     + + + END SPECIFICATIONS + + +
C
 100  CONTINUE
C       legend, line type, color, symbol, shade, mean/point, which axis
        NCRV = NVAR
        DO 115 K = 1,NVAR
C         legend
          CALL ZIPC (L80,BLNK,BUFF(1,K))
          DO 110 I = 1,20
            J = I + 42
            LBC(I,K) = LBV(I,K)
            BUFF(J,K) = LBC(I,K)
 110      CONTINUE
C
          IDUM(1,1,K) = LNTYP(K)+ 1
          IDUM(2,1,K) = COLOR(K)+ 1
          IDUM(3,1,K) = SYMBL(K)+ 1
          IDUM(4,1,K) = PATTRN(K)+ 1
          M = DTYPE(K)
          IF (M.LT.1 .OR. M.GT.2) M = 1
          IDUM(5,1,K) = M
          M = WHICH(K)
          IF (M .LT. 1 .OR. M .GT. 3) M = 1
          IDUM(6,1,K) = M
 115    CONTINUE
C
 200    CONTINUE
          LPTH = 1
          CALL ZWNSOP ( LPTH, WNDNAM )
          IF (NVAR .EQ. 1) THEN
C           only 1 curve, use prm1 screen (don't need axis & legend)
            IC = 5
            SGRP = 20
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL QSETCO ( IC, IDUM )
            CALL Q1EDIT ( IRET )
            CALL QGETCO ( IC, IDUM )
          ELSE
C           more than 1 curve, use prm2 table screen
            IC = 7
            SGRP = 21
            CALL QRESCX (MESSFL,SCLU,SGRP,L1,L1,IC,NVAR,SCNFG,
     M                   LD,R,IDUM,BUFF)
            CALL ZGTRET (IRET)
          END IF
          IF (IRET .EQ. -1) THEN
C           "Oops", try again
            AGAIN = 2
          ELSE IF (IRET .NE. 1) THEN
C           probably "Prev" (2), back to Modify menu
            AGAIN = 0
          ELSE
C           user wants to continue, initialize flags
            FLGVT = 0
            FLGLS = 0
            FLGAY = 1
            FLGAN = 0
            FLGRY = 1
            FLGRN = 0
            DO 225 K = 1,NVAR
C             convert sequence numbers to match TERM.DAT file
              LNTYP(K) = IDUM(1,1,K)- 1
              COLOR(K) = IDUM(2,1,K)- 1
              SYMBL(K) = IDUM(3,1,K)- 1
              PATTRN(K)= IDUM(4,1,K)- 1
              DTYPE(K) = IDUM(5,1,K)
              IF (NVAR.GT.1) THEN
                WHICH(K) = IDUM(6,1,K)
                IF (WHICH(K) .EQ. 1) THEN
C                 left y-axis
                  TRANSF(K) = YTYPE(1)
                ELSE IF (WHICH(K) .EQ. 2) THEN
C                 right y-axis
                  TRANSF(K) = YTYPE(2)
                ELSE IF (WHICH(K) .EQ. 3) THEN
C                 auxiliary axis, always arith
                  TRANSF(K) = 1
                END IF
                DO 220 I=1,20
C                 legend
                  J = I - 1 + IDUM(7,2,K)
                  LBV(I,K) = BUFF(J,K)
                  LBC(I,K)= BUFF(J,K)
 220            CONTINUE
              ELSE
C               one variable
                WHICH(1) = 1
                TRANSF(1) = YTYPE(1)
                CALL CHRCHR (L20,OV,LBV)
              END IF
C             set up type of curve
              IF (CTYPE(K) .NE. 5) THEN
C               not variable time
                IF (NVAR .GT. 1) THEN
                  IF (WHICH(K).EQ.3) THEN
C                   for auxilary axis
                    IF (PATTRN(K) .EQ. 1) THEN
                      CTYPE(K)= 4
                    ELSE
                      CTYPE(K)= 3
                    END IF
                  ELSE IF (PATTRN(K) .EQ. 1) THEN
C                   bar graph on main axes
                    CTYPE(K)= 2
                  ELSE
C                   regular plot with lines or symbols
                    CTYPE(K)= 1
                  END IF
                ELSE
                  IF (PATTRN(K) .EQ. 1) THEN
C                   bar graph on main axes
                    CTYPE(1)= 2
                  ELSE
C                   regular plot with lines or symbols
                    CTYPE(1)= 1
                  END IF
                END IF
              ELSE
C               variable time
                IF (WHICH(K) .EQ. 3) THEN
C                 variable time can only be plotted on main plot
                  FLGVT = 1
                  WHICH(K) = 1
                  CTYPE(K) = 1
                END IF
              END IF
C             check that values for curve K are ok
              IF (LNTYP(K).EQ.0 .AND. SYMBL(K).EQ.0) THEN
C               no line or symbol drawn
                FLGLS = 1
              END IF
              IF (WHICH(K) .EQ. 3) THEN
C               there is a variable for the auxilary axis
                FLGAY = 0
                If (ALEN .LT. 0.001) THEN
C                 aux not selected, REVISE TITLES before PLOTTING.
                  FLGAN = 1
                END IF
              ELSE IF (WHICH(K) .EQ. 2) THEN
C               there is a variable for the right y-axis
                FLGRY = 0
                IF (YTYPE(2) .LT. 1) THEN
C                 right y-axis not previously selected
                  FLGRN = 1
                END IF
              END IF
 225        CONTINUE
C           check for warning and error flags
            INIT = 1
            IWRT = -1
            LPTH = 1
            CALL ZWNSOP ( LPTH, WNDNAM )
            IF (FLGVT .EQ. 1) THEN
C             variable time can only be plotted on main plot
              SGRP = 30
              CALL PMXCNW (MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L)
              CALL ZMNSST
              INIT = 0
            END IF
            IF (FLGLS .EQ. 1) THEN
C             no line or symbol drawn
              SGRP = 31
              CALL PMXCNW (MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L)
              CALL ZMNSST
              INIT = 0
            END IF
            IF (FLGAY .EQ. 1  .AND.  ALEN .GT. 0.001) THEN
C             aux axis selected, but no variables
              SGRP = 32
              CALL PMXCNW (MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L)
              CALL ZMNSST
              INIT = 0
            END IF
            IF (FLGAN .EQ. 1) THEN
C             variable for aux axis, but not previously selected
              SGRP = 33 
              CALL PMXCNW (MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L)
              CALL ZMNSST
              INIT = 0
            END IF
            IF (FLGRY .EQ. 1  .AND.  YTYPE(2) .GT. 0) THEN
C             right y-axis selected, but no variable
              SGRP = 34
              CALL PMXCNW (MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L)
              CALL ZMNSST
              INIT = 0
            END IF
            IF (FLGRN .EQ. 1) THEN
C             variable for right y, but not previously selected
              SGRP = 35
              CALL PMXCNW (MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L)
              CALL ZMNSST
              INIT = 0
            END IF
            IF (INIT .EQ. 0) THEN
C             does the user want to 1-change curves 2-accept defaults
              SGRP = 36
              RESP = 2
              CALL QRESP (MESSFL,SCLU,SGRP,RESP)
              IF (RESP .EQ. 1) THEN
C               look at curves window again
                AGAIN = 1
              ELSE
C               go with defaults
                IF (FLGAY .EQ. 1  .AND.  ALEN .GT. 0.001) THEN
C                 don't include aux axis
                  ALEN = 0.0
                ELSE IF (FLGAN .EQ. 1) THEN
C                 do include aux axis
                  ALEN = 1.0
                  TYPE = 1
                  SCLE = 3
                  CALL SCALEM ( TYPE, SCLE, NVAR, WHICH, YMIN, YMAX,
     O                          PLMN(3), PLMX(3) )
                  TICS(3) = 2
                END IF
                IF (FLGRY .EQ. 1) THEN
C                 don't include right y-axis
                  YTYPE(2) = 0
                ELSE IF (FLGRN .EQ. 1) THEN
C                 do include an arithmetic right y-axis
                  YTYPE(2) = 1
                  SCLE = 2
                  CALL SCALEM ( YTYPE(2), SCLE, NVAR,
     I                          WHICH, YMIN, YMAX,
     O                          PLMN(2), PLMX(2) )
                  TICS(2) = 10
                END IF
                AGAIN = 0
              END IF
            ELSE
C             no warning or error messages
              AGAIN = 0
            END IF
          END IF
        IF (AGAIN .EQ. 2) GO TO 200
      IF (AGAIN .EQ. 1) GO TO 100
C               
      RETURN
      END
C
C
C
      SUBROUTINE   PINCXY
     I                   ( MESSFL, SCLU, WNDNAM )
C
C     + + + PURPOSE + + +
C     Allows user to input or mofdify the specifications for each curve
C     in an x-y plot, including line type, symbol, pattern, color, and
C     labels for the legend.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - keystrokes
C
C      + + + PARAMETERS
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP, AGAIN, IRET, SCNFG, JUST, LSTAT,
     $             I, J, K, K1, K2, L, M, LPTH, OLEN,
     $             C, L0, L1, L2, L3, L8, L20, L40, L78, L80,
     $             IDUM5(5,3,TSMAX), NX(2,TSMAX) 
      REAL         R(2,TSMAX)
      CHARACTER*1  BLNK, BUFF(80,TSMAX)
      CHARACTER*78 C78, B78
C
C     + + + INTRINSICS + + +
      INTRINSIC    MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL     INTCHR, CARVAR, CHRCHR, ZIPC
      EXTERNAL     Q1INIT, QSETI,  QSTCOB, Q1EDIT, QGETI,  QGTCOB
      EXTERNAL     QRESPI, QRESCX, ZGTRET, ZSTADD, ZSTADQ
      EXTERNAL     ZWNSOP, ZQUIET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SCNFG, JUST, BLNK
     $     /    1,    0,  ' ' /
      DATA  L0, L1, L2, L3, L8, L20, L40, L78, L80
     $     / 0,  1,  2,  3,  8,  20,  40,  78,  80 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NCRV .EQ. 0) THEN
C       Number of curves?
        SGRP= 40
        LPTH = 1
        CALL ZWNSOP ( LPTH, WNDNAM )
        CALL QRESPI (MESSFL,SCLU,SGRP,NCRV)
      END IF
C
 100  CONTINUE
C       variable on which axes not known, create status block
        LSTAT = 0
        L = 1
        DO 140 J= 1,NVAR
          IF (MOD(J,2) .EQ. 0) THEN
            K1 = 40
            K2 = 54
          ELSE
            K1 = 1
            K2 = 14
          END IF
          CALL ZIPC (L40,BLNK,BUFF(K1,1))
          CALL INTCHR (J,L8,JUST,OLEN,BUFF(K1,1))
          CALL CHRCHR (L20,LBV(1,J),BUFF(K2,1))
          CALL CARVAR (L78,BUFF,L78,C78)
          IF (J .GT. 10) THEN
C           do nothing
            L = 10
          ELSE IF (J .EQ. NVAR .OR. J .EQ. 10) THEN
            CALL ZSTADD (L, C78)
          ELSE IF (MOD(J,2) .EQ. 0) THEN
            CALL ZSTADQ (L,C78,L0)
            L = L + 1
          END IF
 140    CONTINUE
        LSTAT = L
C
C       # x-axis,# y-axis, left or right, line type, color,
C       symbol, legend
        DO 150 K = 1,NCRV
          CALL ZIPC (L80,BLNK,BUFF(1,K))
C         set which variable for which axis
          IF (WCHVR(1,K).LT.0 .OR. WCHVR(1,K).GT.TSMAX) THEN
C           not defined, set to defaults
            WCHVR(1,K)= 1
          END IF
          IF (WCHVR(2,K).LT.0 .OR. WCHVR(2,K).GT.TSMAX) THEN
C           not defined, set to defaults
            WCHVR(2,K)= 2
          END IF
          NX(1,K) = WCHVR(2,K)
          NX(2,K) = WCHVR(1,K)
C
          IF (WHICH(WCHVR(1,K)) .EQ. 2) THEN
C           right y-axis
            IDUM5(1,1,K) = 2
          ELSE
C           left y-axis
            IDUM5(1,1,K) = 1
          END IF
          CALL CHRCHR ( L20, LBC(1,K), BUFF(43,K) )
          IDUM5(2,1,K) = LNTYP(K)+ 1
          IDUM5(3,1,K) = COLOR(K)+ 1
          IDUM5(4,1,K) = SYMBL(K)+ 1
 150    CONTINUE
C
 200    CONTINUE
          LPTH = 1
          CALL ZWNSOP ( LPTH, WNDNAM )
          IF (NCRV .EQ. 1) THEN
C           only 1 curve, use prm1 screen
            SGRP = 41
            CALL Q1INIT (MESSFL, SCLU, SGRP)
            CALL QSETI  (L2,NX)
            CALL QSTCOB (L1,L1,IDUM5(2,1,1))
            CALL QSTCOB (L1,L2,IDUM5(3,1,1))
            CALL QSTCOB (L1,L3,IDUM5(4,1,1))
            CALL Q1EDIT (IRET)
            IF (IRET .EQ. 1) THEN
              CALL QGETI  (L2,NX)
              CALL QGTCOB (L1,L1,IDUM5(2,1,1))
              CALL QGTCOB (L1,L2,IDUM5(3,1,1))
              CALL QGTCOB (L1,L3,IDUM5(4,1,1))
            END IF
          ELSE
C         more than 1 curve, use prm2 table screen
            I = 2
            C = 5
            SGRP = 42
            CALL QRESCX (MESSFL,SCLU,SGRP,I,L1,C,NCRV,SCNFG,
     M                   NX,R,IDUM5,BUFF)
C           get user exit command
            CALL ZGTRET (IRET)
          END IF
          IF (IRET .EQ. -1) THEN
C           "Oops", try again
            AGAIN = 2
          ELSE IF (IRET .NE. 1) THEN
C           probably "Prev" (2), back to Modify menu
            AGAIN = 0
          ELSE
C           user wants to continue
            AGAIN = 0
            DO 260 K = 1,NCRV
C             L= variable # for x-axis,
C             M = variable # for y-axis
              WCHVR(2,K) = NX(1,K)
              WCHVR(1,K) = NX(2,K)
              L= WCHVR(2,K)
              M= WCHVR(1,K)
              WHICH(L) = 4
              TRANSF(WCHVR(2,K)) = XTYPE
              IF (IDUM5(1,1,K) .EQ.1) THEN
C               left y-axis
                WHICH(M) = 1
                TRANSF(WCHVR(1,K)) = YTYPE(1)
              ELSE
C               right y-axis
                WHICH(M) = 2
                TRANSF(WCHVR(1,K)) = YTYPE(2)
              END IF
              LNTYP(K)= IDUM5(2,1,K)- 1
              COLOR(K)= IDUM5(3,1,K)- 1
              SYMBL(K)= IDUM5(4,1,K)- 1
              CTYPE(K)= 6
C
              IF (NCRV .GT. 1) THEN
C               uses curve names
                CALL CHRCHR (L20,BUFF(43,K),LBC(1,K))
              END IF
 260        CONTINUE
C
            IF (LSTAT .GT. 0) THEN
C             delete status block
              CALL ZIPC ( L78, BLNK, B78 )
              DO 275 I = 1,LSTAT
                CALL ZSTADQ(I,B78,L0)
 275          CONTINUE
              CALL ZQUIET
            END IF
          END IF
        IF (AGAIN .EQ. 2) GO TO 200
C       go to 100 is here to allow for later addion of checking kmf
C       such as checking that a curve has a line type or a symbol.
      IF (AGAIN .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINSCL
     I                   ( MESSFL, SCLU, WNDNAM )
C
C     + + + PURPOSE + + +
C     Allow the user to set or modify the scales and the action to be
C     taken if any points will be off the scale.
C
C     + + + HISTORY + + +
C     kmf 01/05/16  force probability x-axis to have symetrical bounds
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - keystrokes
C
C     + + + PARAMETERS + + +
      INTEGER   OPTS
      PARAMETER ( OPTS=6 )
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP(OPTS), NFLD(OPTS), LENR(OPTS), LENT(OPTS),
     $             LENB(OPTS), LNAN(OPTS), LNAL(OPTS), CBAS,
     $             LENC(OPTS), BLOC(12,OPTS), POS(8,OPTS), BUFC(4),
     $             IMIN(3), IMAX(3), IDEF(3), DNUM,
     $             VLINFG(22,OPTS), UCCNT(22,OPTS), UCDEF(7,OPTS),
     $             USTRLN(22,OPTS),
     $             AGAIN, LPTH, OPT, CLEN(3), L, K, LEN, LEN20,
     $             BUFI(3), TYPE, RTCMND
      REAL         RMIN(12), RMAX(12), RDEF(12), MMIN(4), MMAX(4),
     $             BUFR(12)
      DOUBLE PRECISION DMIN(1), DMAX(1), DDEF(1)
      CHARACTER*1  BUFA(60), URSPST(960), BLNK
      CHARACTER*20 DESCY(2,3), DESCX(3)
      CHARACTER*48 VBAD
C
C     + + + LOCAL DEFINITIONS + + +
C     OPT    - option for axis included
C              1 - left                   4 - left & right
C              2 - left & aux             5 - left & bottom
C              3 - left, right & aux      6 - left, right & bottom
C     NFLD   - number of fields for each option (LENT+LENR+LENC)
C     LENR   - number of real fields for scale and data ranges
C     LENT   - number of fields for tic marks
C     LENB   - number of fields for handling off-scale (bad) values
C     LNAN   - number of fields for axis description
C     LENC   - total number of character fields (LENB+LNAN)
C     LNAL   - total number of characters in axis descriptions
C     BLOC   - location in buffers for scale ranges, tics, and off-scale
C     POS    - location in buffers for data range and axis descriptions
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS, MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL   OPTNAX, PLSCLI, SCALIT
      EXTERNAL   ZWNSOP, QSCSET, Q1INIT, Q1EDIT
      EXTERNAL   QSETR,  QSETI,  QSETCO, QSTCTB
      EXTERNAL   QGETR,  QGETI,  QGETCO
      EXTERNAL   COPYI,  CVARAR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SGRP / 111, 112, 113, 114, 115, 116 /
      DATA  NFLD /   8,  14,  20,  14,  16,  22 /
      DATA  LENR /   4,   8,  12,   8,   8,  12 /
      DATA  LENT /   1,   2,   3,   2,   2,   3 /
      DATA  LENB /   2,   2,   2,   2,   4,   4 /
      DATA  LNAN /   1,   2,   3,   2,   2,   3 /
      DATA  LENC /   3,   4,   5,   4,   6,   7 /
      DATA  LNAL /  20,  40,  60,  40,  40,  60 /
C                 <--- plmn ----> <--- tics ----> <--- bvalfg -->
C                 (1) (2) (3) (4) (1) (2) (3) (4) (1) (2) (3) (4)
      DATA  BLOC / 1,  0,  0,  0,  1,  0,  0,  0,  1,  2,  0,  0,
     $             1,  0,  3,  0,  1,  0,  2,  0,  1,  2,  0,  0,
     $             1,  3,  5,  0,  1,  2,  3,  0,  1,  2,  0,  0,
     $             1,  3,  0,  0,  1,  2,  0,  0,  1,  2,  0,  0,
     $             1,  0,  0,  3,  1,  0,  0,  2,  1,  2,  3,  4,
     $             1,  3,  0,  5,  1,  2,  0,  3,  1,  2,  3,  4 /
C                 <--- mmin ----> <--- desc ---->
C                 (1) (2) (3) (4) (1) (2) (3) (4)
      DATA  POS  / 3,  0,  0,  0,  1,  0,  0,  0,
     $             5,  0,  7,  0,  1,  0, 21,  0,
     $             7,  9, 11,  0,  1, 21, 41,  0,
     $             5,  7,  0,  0,  1, 21,  0,  0,
     $             5,  0,  0,  7,  1,  0,  0, 21,
     $             7,  9,  0, 11,  1, 21,  0, 41 /
      DATA  IMIN, IMAX, IDEF, DNUM, DMIN, DMAX, DDEF
     $    /  3*0,  3*0,  3*0,    1,    0,    0,    0 /
      DATA  CLEN, LEN20, BLNK
     $    / 3*20,    20,  ' ' /
      DATA  DESCY / '<-- Left-Y arith -->', '<--- Left-Y log --->',
     $              '<- Right-Y arith -->', '<--- Right-Y log -->',
     $              '<---- Auxiliary --->', '<---- Auxiliary --->' /
      DATA  DESCX / '<-- X arithmetic -->', '<- X logarithmic -->',
     $              '<- X probability -->' /
      DATA  VBAD  / 'CLIP  SKIP  ARROW IGNORECLIP  SKIP  ARROW IGNORE' /
C                  scale  tic  bound  desc range
      DATA VLINFG / 2*0,    0,  2*1,    0,  2*0,  14*0,
     $              4*0,  2*0,  2*1,  2*0,  4*0,  8*0,
     $              6*0,  3*0,  2*1,  3*0,  6*0,  2*0,
     $              4*0,  2*0,  2*1,  2*0,  4*0,  8*0,
     $              4*0,  2*0,  4*1,  2*0,  4*0,  6*0,
     $              6*0,  3*0,  4*1,  3*0,  6*0       /
C                  bound  desc           bound  desc
      DATA  UCDEF / 2*4,    0,  4*0,      2*4,  2*0,  3*0,
     $              2*4,  3*0,  2*0,      2*4,  2*0,  3*0,
     $              4*4,  2*0,    0,      4*4,  3*0       /
C                        bound                 bound
      DATA  UCCNT / 3*0,  2*4, 17*0,      6*0,  2*4, 14*0,
     $              9*0,  2*4, 11*0,      6*0,  2*4, 14*0,
     $              6*0,  4*4, 12*0,      9*0,  4*4,  9*0 /
C                        bound                 bound
      DATA USTRLN / 3*0,  2*6, 17*0,      6*0,  2*6, 14*0,
     $              9*0,  2*6, 11*0,      6*0,  2*6, 14*0,
     $              6*0,  4*6, 12*0,      9*0,  4*6,  9*0 /
C
C     + + + DEBUG FORMATS + + +
C3001 format (//,'  <-- pinscl: xtype=', i5,
C    $         /,'              plmx =', 4f10.2,
C    $         /,'              plmn =', 4f10.2 )
C3002 format (   '              plmx =', 4f10.2,
C    $         /,'              plmn =', 4f10.2 )
C
C     + + + END SPECIFICATIONS + + +
C
C     get option for axis to determine required scales
      CALL OPTNAX ( XTYPE, YTYPE, ALEN, OPT )
C
C     determine minimum and maximum of all curves on axis
      DO 150 L = 1, 4
        IF (BLOC(L,OPT) .NE. 0) THEN
C         this axis is include, initialize limits
          MMIN(L) = 1.0E10
          MMAX(L) = -1.0E10
          DO 100 K = 1, NVAR
            IF (WHICH(K) .EQ. L) THEN
C             variable K is on axis L
              IF (YMAX(K) .GT. MMAX(L)) MMAX(L) = YMAX(K)
              IF (YMIN(K) .LT. MMIN(L)) MMIN(L) = YMIN(K)
            END IF
 100      CONTINUE
        END IF
 150  CONTINUE
C
 200  CONTINUE
C       handling for off-scale values (1-clip,2-skip,3-arrow,4-ignore)
        LEN = 960
        CALL ZIPC ( LEN, BLNK, URSPST )
        LEN = 48
        CBAS = 3
        CALL CVARAR ( LEN, VBAD, LEN, URSPST )
        IF (LENB(OPT) .EQ. 4) THEN
C         include left and right bounds
          CBAS = 5
          CALL CVARAR ( LEN, VBAD, LEN, URSPST(LEN+1) )
        END IF
        CALL COPYI ( LENB(OPT), BVALFG, BUFC(BLOC(9,OPT)) )
C
C       always include left y-axis
        IF (YTYPE(1) .EQ. 1) THEN
C         arithmatic axis, include tic marks
          CALL CVARAR ( LEN20, DESCY(1,1), LEN20, BUFA(POS(5,OPT)) )
          BUFI(BLOC(5,OPT)) = TICS(1)
        ELSE
C         log axis, tic marks not applicable
          CALL CVARAR ( LEN20, DESCY(2,1), LEN20, BUFA(POS(5,OPT)) )
          BUFI(BLOC(5,OPT)) = 0
        END IF
        CALL PLSCLI ( YTYPE(1), BLOC(5,OPT), BLOC(1,OPT), POS(1,OPT),
     O                IMIN, IMAX, IDEF, RMIN, RMAX, RDEF )
C       calcualte recommended scale
        CALL SCALIT ( YTYPE(1), MMIN(1), MMAX(1), PLMN(1), PLMX(1) )
C       put recommended scale and actual limits
        BUFR(BLOC(1,OPT))   = PLMN(1)
        BUFR(BLOC(1,OPT)+1) = PLMX(1)
        BUFR(POS(1,OPT))    = MMIN(1)
        BUFR(POS(1,OPT)+1)  = MMAX(1)
C
        IF (BLOC(2,OPT) .NE. 0) THEN 
C         right y-axis is included
          IF (YTYPE(2) .EQ. 1) THEN
C           arithmatic axis, includes tic marks
            CALL CVARAR ( LEN20, DESCY(1,2), LEN20, BUFA(POS(6,OPT)) )
            BUFI(BLOC(6,OPT)) = TICS(2)
          ELSE
C           log axis, tic marks not applicable
            CALL CVARAR ( LEN20, DESCY(2,2), LEN20, BUFA(POS(6,OPT)) )
            BUFI(BLOC(6,OPT)) = 0
          END IF
          CALL PLSCLI ( YTYPE(2), BLOC(6,OPT), BLOC(2,OPT), POS(2,OPT),
     O                  IMIN, IMAX, IDEF, RMIN, RMAX, RDEF )
C         calculate recommended scale
          CALL SCALIT ( YTYPE(2), MMIN(2), MMAX(2), PLMN(2), PLMX(2) )
C         put recommended scale and actual limits
          BUFR(BLOC(2,OPT))   = PLMN(2)
          BUFR(BLOC(2,OPT)+1) = PLMX(2)
          BUFR(POS(2,OPT))    = MMIN(2)
          BUFR(POS(2,OPT)+1)  = MMAX(2)
        END IF
C
        IF (BLOC(3,OPT) .NE. 0) THEN
C         auxiliary axis is included, always arithmatic
          CALL CVARAR ( LEN20, DESCY(1,3), LEN20, BUFA(POS(7,OPT)) )
          BUFI(BLOC(7,OPT)) = TICS(3)
          TYPE = 1
          CALL PLSCLI ( TYPE, BLOC(7,OPT), BLOC(3,OPT), POS(3,OPT),
     O                  IMIN, IMAX, IDEF, RMIN, RMAX, RDEF )
C         calculate recommended scale
          CALL SCALIT ( TYPE, MMIN(3), MMAX(3), PLMN(3), PLMX(3) )
C         put recommended scale and actual limits
          BUFR(BLOC(3,OPT))   = PLMN(3)
          BUFR(BLOC(3,OPT)+1) = PLMX(3)
          BUFR(POS(3,OPT))    = MMIN(3)
          BUFR(POS(3,OPT)+1)  = MMAX(3)
        END IF
C
        IF (BLOC(4,OPT) .NE. 0) THEN
C         x axis is included
          IF (XTYPE .EQ. 1) THEN
C           arithmatic axis, includes tic marks
            CALL CVARAR ( LEN20, DESCX(1), LEN20, BUFA(POS(8,OPT)) )
            BUFI(BLOC(8,OPT)) = TICS(4)
          ELSE IF (XTYPE .EQ. 2) THEN
C           log axis, tic marks not applicable
            CALL CVARAR ( LEN20, DESCX(2), LEN20, BUFA(POS(8,OPT)) )
            BUFI(BLOC(8,OPT)) = 0
          ELSE
C           assume probability axis, tic marks not applicable
            CALL CVARAR ( LEN20, DESCX(3), LEN20, BUFA(POS(8,OPT)) )
            BUFI(BLOC(8,OPT)) = 0
          END IF
          CALL PLSCLI ( XTYPE, BLOC(8,OPT), BLOC(4,OPT), POS(4,OPT),
     O                  IMIN, IMAX, IDEF, RMIN, RMAX, RDEF )
C         calculate recommended scale
          CALL SCALIT ( XTYPE, MMIN(4), MMAX(4), PLMN(4), PLMX(4) )
C         put recommended scale and actual limits
          BUFR(BLOC(4,OPT))   = PLMN(4)
          BUFR(BLOC(4,OPT)+1) = PLMX(4)
          BUFR(POS(4,OPT))    = MMIN(4)
          BUFR(POS(4,OPT)+1)  = MMAX(4)
        END IF
C
C       set the valid min, max, def for the responses
        LPTH = 1
        CALL ZWNSOP ( LPTH, WNDNAM )
        CALL QSCSET ( LENT(OPT), LENR(OPT), DNUM, LENC(OPT), NFLD(OPT),
     I                IMIN, IMAX, IDEF, RMIN, RMAX, RDEF,
     I                DMIN, DMAX, DDEF,
     I                VLINFG(1,OPT), UCCNT(1,OPT), UCDEF(1,OPT),
     I                USTRLN(1,OPT), URSPST )
C
C       initialize question and fill in current values
        CALL Q1INIT ( MESSFL, SCLU, SGRP(OPT) )
        CALL QSETR ( LENR(OPT), BUFR )
        CALL QSETI ( LENT(OPT), BUFI )
        CALL QSETCO ( LENB(OPT), BUFC )
        CALL QSTCTB ( LNAN(OPT), CLEN, LNAL(OPT),
     I                CBAS, BUFA(POS(5,OPT)) )

C       edit the screen
        CALL Q1EDIT ( RTCMND )
C
        IF (RTCMND .EQ. -1) THEN
C         user "Oops"ed, try again
          AGAIN = 1
        ELSE IF (RTCMND .EQ. 1) THEN
C         user "Accept"ed screen, get user's responses
          CALL QGETR ( LENR(OPT), BUFR )
          CALL QGETI ( LENT(OPT), BUFI )
          CALL QGETCO ( LENB(OPT), BUFC )
C
C         handling for off-scale (bad) values
          CALL COPYI ( LENB(OPT), BUFC(BLOC(9,OPT)), BVALFG )
C         left y-axis is always included
          TICS(1) = BUFI(BLOC(5,OPT))
          PLMN(1) = BUFR(BLOC(1,OPT))
          PLMX(1) = BUFR(BLOC(1,OPT)+1)
C
          IF (BLOC(2,OPT) .NE. 0) THEN
C           right y-axis is included
            TICS(2) = BUFI(BLOC(6,OPT))
            PLMN(2) = BUFR(BLOC(2,OPT))
            PLMX(2) = BUFR(BLOC(2,OPT)+1)
          END IF
C
          IF (BLOC(3,OPT) .NE. 0) THEN
C           auxiliary axis is included
            TICS(3) = BUFI(BLOC(7,OPT))
            PLMN(3) = BUFR(BLOC(3,OPT))
            PLMX(3) = BUFR(BLOC(3,OPT)+1)
          END IF
C
          IF (XTYPE .GT. 0) THEN
C           x axis is include
            TICS(4) = BUFI(BLOC(8,OPT))
            PLMN(4) = BUFR(BLOC(4,OPT))
            PLMX(4) = BUFR(BLOC(4,OPT)+1)
            IF (XTYPE .GT. 2) THEN
Ckmf          probability, force limits to be symetrical
C             write (99,3001) xtype, plmx, plmn
              IF ( ABS ( ABS(PLMX(4))-ABS(PLMN(4)) ) .GT. .0001) THEN
                PLMX(4) = MAX ( ABS(PLMX(4)), ABS(PLMN(4)) )
                PLMN(4) = -PLMX(4)
C               write (99,3002) plmx, plmn
              END IF
            END IF
Ckmf        end addition May 16, 2001 kmf
          END IF
          AGAIN = 0
        ELSE
C         user did not "Accept" or "Oops", probably "Prev"
          AGAIN = 0
        END IF
      IF (AGAIN .EQ. 1) GO TO 200
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINEXA
     I                   (MESSFL,SCLU,WNDNAM)
C
C     + + + PURPOSE + + +
C     This routine contains the Extra options to allow the user to add
C     a block of text to a plot, positon the legend on the plot, and
C     designate a window to be blanked out so that text can be written
C     to the plot.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - keystrokes
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RESP, OPT, IDUM(2), SGRP, AGAIN, IRET, LPTH,
     $          L2, L4, LMAX, LL(2), NL, L1, LGDOPT
      REAL      RDUM(2), TLCLGD(2)
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  Q1INIT, QSETR, QSETI, QSETCT
      EXTERNAL  Q1EDIT, QGETR, QGETI, QGETCT, QSETCO, QGETCO
      EXTERNAL  ZWNSOP, QRESP, PRNTXT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L1, L2, L4, LMAX, LL
     $     / 1,  2,  4,  120, 60,60 /
C
C     + + + END SPECIFICATIONS + + +
C
 50   CONTINUE
C       Extra option: 1-return,2-text,3-legend,4-blanking
        LPTH = 1
        CALL ZWNSOP ( LPTH, WNDNAM )
        IF (XTYPE .GT. 0  .AND.  NCRV .GT. 1) THEN
C         not time plot, include all options
          SGRP = 70
          CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
          OPT = RESP
        ELSE IF (NCRV .GT. 1  .AND.  XTYPE .EQ. 0) THEN
C         time plot, include options 1, 2, and 3, but no blanking
          SGRP = 71
          CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
          OPT = RESP
        ELSE IF (NCRV .LE. 1  .AND.  XTYPE .EQ. 1) THEN
C         include options 1, 2, and 4, but no legend
          SGRP = 72
          CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
          OPT = RESP
          IF (OPT .EQ. 3) OPT = 4
        ELSE IF (NCRV .LE. 1  .AND.  XTYPE .EQ. 0) THEN
C         include options 1 and 2, but no legend or blanking
          SGRP = 73
          CALL QRESP ( MESSFL, SCLU, SGRP, OPT )
          OPT = RESP
        END IF
        IF (OPT .EQ. 2) THEN
 200      CONTINUE
C           adding text, get location and text
            RDUM(1) = FXT
            RDUM(2) = FYT
            IDUM(1) = CPR
            IF (CPR .NE. 0) THEN
              IDUM(2) = NCHR/CPR
            ELSE
              IDUM(2) = -999
            END IF
            SGRP = 75
            LPTH = 1
            CALL ZWNSOP ( LPTH, WNDNAM )
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL QSETR ( L2, RDUM )
            CALL QSETI ( L2, IDUM )
            CALL QSETCT ( L2, LL, LMAX, CTXT )
            CALL Q1EDIT ( IRET )
            IF (IRET .EQ. -1) THEN
C             "Oops", try again
              AGAIN = 1
            ELSE IF (IRET .EQ. 1) THEN
C             user wants the values
              CALL QGETR ( L2, RDUM )
              CALL QGETI ( L2, IDUM )
              NL = IDUM(2)
              CPR = IDUM(1)
              NCHR= NL*CPR
              FXT = RDUM(1)
              FYT = RDUM(2)
              CALL QGETCT ( L2, LL, LMAX, CTXT )
C             now check it out
              IF (NCHR .GT. LMAX ) THEN
C               too many characters, maximum is 120
                SGRP = 76
                LPTH = 1
                CALL ZWNSOP ( LPTH, WNDNAM )
                CALL PRNTXT ( MESSFL, SCLU, SGRP )
                AGAIN = 1
              END IF
            ELSE
C             assume "Prev"ious
              AGAIN = 0
            END IF
          IF (AGAIN .EQ. 1) GO TO 200
        ELSE IF (OPT .EQ. 3) THEN
C         set location of legend
 300      CONTINUE
            IF (LOCLGD(1) .LT. -1.9) THEN
C             legend not to be used so set to default
              TLCLGD(1)= 0.05
              TLCLGD(2)= 0.95
              LGDOPT = 3
            ELSE IF (LOCLGD(1) .LT. -0.999) THEN
C             use default legend
              TLCLGD(1)= 0.05
              TLCLGD(2)= 0.95
              LGDOPT = 2
            ELSE
C             use current values
              TLCLGD(1) = LOCLGD(1)
              TLCLGD(2) = LOCLGD(2)
              IF (ABS(TLCLGD(1)-0.05).LT. 0.0001 .AND.
     &            ABS(TLCLGD(2)-0.95).LT. 0.0001) THEN
C               must be default
                LGDOPT = 2
              ELSE
C               must be custom
                LGDOPT = 1
              END IF
            END IF
            SGRP = 77
            LPTH = 1
            CALL ZWNSOP ( LPTH, WNDNAM )
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALl QSETR ( L2, TLCLGD )
            CALL QSETCO ( L1, LGDOPT )
            CALL Q1EDIT ( IRET )
            IF (IRET .EQ. -1) THEN
C             "Oops", try again
              AGAIN = 1
            ELSE IF (IRET .EQ. 1) THEN
C             user wants the values
              CALL QGETCO (L1, LGDOPT)
              IF (LGDOPT .EQ. 1) THEN
C               custom
                CALL QGETR ( L2, LOCLGD )
              ELSE IF (LGDOPT .EQ. 2) THEN
C               default
                LOCLGD(1) = 0.05
                LOCLGD(2) = 0.95
              ELSE
C               no legend
                LOCLGD(1) = -2.0
                LOCLGD(2) = -2.0
              END IF
              AGAIN = 0
            ELSE
C             assume "Prev"ious
              AGAIN = 0
            END IF
          IF (AGAIN .EQ. 1) GO TO 300
        ELSE IF (OPT .EQ. 4) THEN
C         define window for blanking
 400      CONTINUE
            SGRP = 78
            LPTH = 1
            CALL ZWNSOP ( LPTH, WNDNAM )
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL QSETR ( L4, BLNKIT )
            CALL Q1EDIT ( IRET )
            IF (IRET .EQ. -1) THEN
C             "Oops", try again
              AGAIN = 1
            ELSE IF (IRET .EQ. 1) THEN
C             user wants the values
              CALL QGETR ( L4, BLNKIT )
              AGAIN = 0
            ELSE
C             assume "Prev"ious
              AGAIN = 0
            END IF
          IF (AGAIN .EQ. 1) GO TO 400
        END IF
      IF (OPT .NE. 1) GO TO 50
C
      RETURN
      END
C
C
C
      SUBROUTINE   PINSIZ
     I                   (MESSFL,SCLU,WNDNAM,
     O                    WNDW)
C
C     + + + PURPOSE + + +
C     This routine is called from PROPLT and will allow the user to
C     modify page size, axis lengths, location of origin, and
C     letter sizes and checks that the sizes are reasonable.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WNDW
      CHARACTER*8 WNDNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WNDNAM - keystrokes
C     WNDW   - flag for change in size of graphics window
C              0 - no change
C              1 - changed, need to close and reopen workstatio
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMOMS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP, RESP, RFLG, IR, LPTH, IRET, INIT, AGAIN, MAXL,
     $            L, NLT, NLX, NLY, L1, L4, L80, L240, CPT,
     $            K, IWRT
      REAL        RDUM(8), TEMP, YMLEN, OLDWND(4)
      CHARACTER*1 NXTLNE(1)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, COPYR
      EXTERNAL    SIZAXE
      EXTERNAL    ZWNSOP, PMXCNW, ZMNSST, QRESP
      EXTERNAL    Q1INIT, QSETR,  Q1EDIT, QGETR,  ANPRGT
C
C     DATA INITIALIZATIONS
      DATA L1,L4,L80,L240,NXTLNE,MAXL
     $    / 1, 4, 80, 240,  '&',  10 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (XLEN .LE. 0.0) THEN
C       compute default sizes if XLEN not set
        NLT= 1
        NLY= 1
        NLX= 1
        K  = STRFND(L240,TITL,L1,NXTLNE)
        IF (K.GT.0) NLT= NLT+ 1
        K  = STRFND(L240-K,TITL(K+1),L1,NXTLNE)
        IF (K.GT.0) NLT= NLT+ 1
        K  = STRFND(L80,YXLABL,L1,NXTLNE)
        IF (K.GT.0) NLX= NLX+ 1
        K  = STRFND(L80,YLABL,L1,NXTLNE)
        IF (K.GT.0) NLY= NLY+ 1
        CALL SIZAXE (XPAGE,YPAGE,NCRV,CTYPE,YTYPE,
     M               NLT,NLX,NLY,
     O               XLEN,YLEN,SIZEL,XPHYS,YPHYS)
      END IF
C
      CALL COPYR ( L4, XWINLC, OLDWND )
      WNDW = 0
 100  CONTINUE
C       Get axes length, letter size and location of origin.
        RFLG = 0
        RDUM(1) = YPAGE
        RDUM(2) = XPAGE
        RDUM(3) = YLEN
        RDUM(4) = XLEN
        RDUM(7) = SIZEL
        RDUM(5) = YPHYS
        RDUM(6) = XPHYS
        RDUM(8) = ALEN
 150    CONTINUE
          AGAIN = 0
          IF (ALEN .GT. 0.0001) THEN
C           include auxilary axis
            IR = 8
            SGRP = 80
          ELSE
C           don't include aux axis
            IR = 7
            SGRP = 81
          END IF
          LPTH = 1
          CALL ZWNSOP ( LPTH, WNDNAM )
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
          CALL QSETR ( IR, RDUM )
          CALL Q1EDIT ( IRET )
          IF (IRET .EQ. -1) THEN
C           user "Oops"ed, try again
            AGAIN = 1
          ELSE IF (IRET .NE. 1) THEN
C           user wants out and back to "Prev"
            AGAIN = 0
          ELSE
C           user has accepted, get the values
            CALL QGETR ( IR, RDUM )
            YPAGE = RDUM(1)
            XPAGE = RDUM(2)
            YLEN = RDUM(3)
            XLEN = RDUM(4)
            SIZEL = RDUM(7)
            YPHYS = RDUM(5)
            XPHYS = RDUM(6)
            IF (ALEN .GT. 0.0001) ALEN = RDUM(8)
C           check that values are ok
            INIT = 1
            IWRT = -1
            RFLG = 0
            LPTH = 1
            CALL ZWNSOP ( LPTH, WNDNAM )
            IF (YLEN+YPHYS .GT. YPAGE) THEN
C             Origin plus y-axis length too big
              SGRP = 82
              CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
              INIT = -1
              RFLG = 1
            END IF
            IF (XLEN+XPHYS .GT. XPAGE) THEN
C             Origin plus x-axis length too big
              SGRP = 83
              CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
              INIT = -1
              RFLG = 1
            END IF
C           estimate space required for y-axis label
            YMLEN = YPAGE- 1.0- 10.0* SIZEL
            DO 175 K = 1, NCRV
C             account for aux axis if appropriate
              IF (CTYPE(K).EQ.3) YMLEN= YPAGE- ALEN- 1.5* SIZEL
 175        CONTINUE
            IF (SIZEL*40.0.GT.YMLEN) THEN
C             letter size may be too big for y-axis
              SGRP = 84
              CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
              INIT = -1
            END IF
            IF (ALEN .GT. 0.0001) THEN
C             check number of tics against aux  axis length
              TEMP = 2.0*SIZEL*REAL(TICS(3))
              IF (TEMP .GT. ALEN) THEN
C               letter size to big for # of tics, reduce size or # tics
                SGRP = 85
                CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
                INIT = -1
              END IF
            END IF
C           any problems or warnings?
            IF (RFLG .EQ. 1) THEN
C             problems with axis lengths must be fixed
              SGRP = 86
              IWRT = 0
              CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
              AGAIN = 1
            ELSE IF (INIT .NE. 1) THEN
C             letter size problems fix them 1-no, 2-yes
              SGRP = 87
              RESP = 1
              CALL ZMNSST
              CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
              IF (RESP .EQ. 1) THEN
C               no, don't fix letters
                AGAIN = 0
                IRET = 1
              ELSE
C               yes, fix letter size
                AGAIN = 1
              END IF
            END IF
          END IF
        IF (AGAIN .EQ. 1) GO TO 150
C
        IF (IRET .EQ. 1) THEN
C         set window position if running under X11
          CALL ANPRGT (L1, CPT)
          IF (CPT .EQ. 5 .AND. DEVCOD .EQ. 1100) THEN
 200        CONTINUE
              SGRP = 90
              LPTH = 1
              CALL ZWNSOP ( LPTH, WNDNAM )
              CALL Q1INIT (MESSFL, SCLU, SGRP)
              CALL QSETR (L4, XWINLC)
              CALL Q1EDIT (IRET)
              IF (IRET .EQ. -1) THEN
C               "Oops", try again
                AGAIN = 1
              ELSE IF (IRET .EQ. 1) THEN
C               make the change
                CALL QGETR (L4, XWINLC)
                AGAIN = 0
              ELSE
C               "Prev" or unknown, presume "Prev"
                AGAIN = 2
              END IF
            IF (AGAIN .EQ. 1) GO TO 200
          END IF
        END IF
      IF (AGAIN .EQ. 2) GO TO 100
C
C     check for change in location of graphics window
      DO 300 K = 1, 4
        IF (ABS(XWINLC(K)-OLDWND(K)) .GE. 0.0001) WNDW = 1
 300  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSETUP
     O                    (RETCOD)
C
C     + + + PURPOSE + + +
C     This routine, called by DOPLOT, does the level one and two
C     calls for GKS to set the scales, establish the plotting space,
C     set the origin, and allow the axes to be labeled by special
C     routines.  The information is read from the users TERM.DAT file
C     (or uses AIDE system defaults) and call the non-AIDE routine
C     GSTUPW. Assumes the workstation id is 1.
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETCOD - return code    0 = everything ok,
C                             1 = device not setup to plot
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   WSID
C
C     + + + EXTERNALS + + +
      EXTERNAL   PSTUPW
C
C     + + + END SPECIFICATIONS + + +
C
      WSID = 1
C
      CALL PSTUPW (WSID, RETCOD)
C
      RETURN
      END
C
C
      SUBROUTINE   PSTUPW
     I                    (WSID,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     This routine, called by DOPLOT, does the level one and two
C     calls for GKS to set the scales, establish the plotting space,
C     set the origin, and allow the axes to be labeled by special
C     routines.  The information is read from the users TERM.DAT file
C     (or uses AIDE system defaults) and call the non-AIDE routine
C     GSTUPW.
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETCOD, WSID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WSID   - workstation (Xwindow) identification number
C     RETCOD - return code    0 = everything ok,
C                             1 = device not setup to plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IC, PRMIND, FONT, PREC, IMET, I, CLRFRC,
     &           JR, IBLUE, IGREEN, IRED, ICHSP, ICHXP,
     &           BCOLOR, L1, FCOLOR
      REAL      CHXP, CHSP
      REAL      RR, RED(2), GREEN(2), BLUE(2), BCB, BCG, BCR
      REAL      CR,CG,CB, FCR,FCG,FCB
      CHARACTER*8 CLRSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT, GSTUPW, GGTGLA, GSCR, GGDVCD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  RED,GREEN,BLUE/0.0,1.0,0.0,1.0,0.0,1.0/, L1/1/
C
C     + + + END SPECIFICATIONS + + +
C
C     find computer type
      PRMIND = 1
      CALL ANPRGT (PRMIND, IC)
C
C     Check for meta files
C     initialize IMET to 0 (no meta files)
      IMET = 0
      IF (IC.EQ.2 .AND. DEVTYP.EQ.4) THEN
C       set GKS or CGM meta file code number for Prime
        IMET = 1
      END IF
      IF (IC.EQ.2 .AND. DEVCOD.EQ.5) THEN
C       set DISSPLA meta file flags
        IMET = 1
      END IF
C
C     get text font and precision
      PRMIND = 47
      CALL ANPRGT (PRMIND, PREC)
      PREC = PREC - 1
      IF (DEVTYP .EQ. 3) THEN
C       plotter
        PRMIND = 50
      ELSE IF (DEVTYP .EQ. 2) THEN
C       printer
        PRMIND = 49
      ELSE
C       screen
        PRMIND = 48
      END IF
      CALL ANPRGT (PRMIND, FONT)
C
C     get expansion factor/char spacing from TERM.DAT
      PRMIND = 103
      CALL ANPRGT (PRMIND, ICHXP)
      PRMIND = 104
      CALL ANPRGT (PRMIND, ICHSP)
      IF (IC .EQ. 2) THEN
C       spacing and expansion for PRIME with GKS bug
        CHXP = 1.0
        CHSP = 0.2
      ELSE
C       use values from TERM.DAT file for all other systems
        CHSP = REAL(ICHSP)/100.0
        CHXP = REAL(ICHXP)/100.0
      END IF
C
C     set background color
      PRMIND = 101
      CALL ANPRGT (PRMIND, BCOLOR)
      IF (BCOLOR .EQ. 1 .OR. BCOLOR .EQ. 2) THEN
C       1 is black, 2 is white, check device code
        CALL GGDVCD ( DEVCOD)
        IF (IC .EQ. 5 .AND. BCOLOR .EQ. 1 .AND. DEVCOD .NE. 1100) THEN
C         special case, DG, non-window device, use white for background
          BCOLOR = 2
          WRITE (99,*) ' Black Background color for non-Xwindow'
          WRITE (99,*) ' Black background changed to white.'
        END IF
        BCR = RED(BCOLOR)
        BCG = GREEN(BCOLOR)
        BCB = BLUE(BCOLOR)
      ELSE
C       input colors from TERM.DAT file for 'other' color
        PRMIND = 105
        CALL ANPRGT (PRMIND, IRED)
        PRMIND = 106
        CALL ANPRGT (PRMIND, IGREEN)
        PRMIND = 107
        CALL ANPRGT (PRMIND, IBLUE)
        BCR = REAL(IRED)/100.0
        BCG = REAL(IGREEN)/100.0
        BCB = REAL(IBLUE)/100.0
      END IF
C
C     Set symbol size ratio
      PRMIND = 102
      CALL ANPRGT(PRMIND,JR)
      RR = REAL(JR)/100.0
C
      CALL GSTUPW (IC,IMET,PREC,FONT,CHXP,CHSP,BCR,BCG,BCB,RR,
     &             WSID,RETCOD)
C
C     color table (values in table start with index of 0 which
C     is for the background which is set in GSTUPW
C
C     set axes and label color as index 1
      PRMIND = 109
      CALL ANPRGT (PRMIND, FCOLOR)
C     check that don't have white on white or black on black
      IF (BCOLOR .EQ. 1 .AND. FCOLOR .EQ. 1) THEN
C       black on black, change index 1 to white
        FCOLOR = 2
      ELSE IF (BCOLOR .EQ. 2 .AND. FCOLOR .EQ. 2) THEN
C       white on white, change index 1 to black
        FCOLOR = 1
C     ELSE
C       background is some color other than white or black, 
C       so do nothing
      END IF
C     set index 1
      FCR = RED(FCOLOR)
      FCG = GREEN(FCOLOR)
      FCB = BLUE(FCOLOR)
      CALL GSCR(WSID,L1,FCR,FCG,FCB)
C     set LBCOLR to color index 1
      LBCOLR = 1 
C    
      CLRSTR= 'COLOR   '
      I= 0
 10   CONTINUE
C       reads term.dat color values and ignores the first two
C       since they are forground and background previously 
C       defined above
        I= I+ 1
        CALL GGTGLA (CLRSTR,I,
     O               CLRFRC)
        IF (CLRFRC .GT. 0 .AND. I.GT.2) THEN
C         update color fraction
          CR= FLOAT     (CLRFRC/65536)      /256.0
          CG= FLOAT(MOD((CLRFRC/256  ),256))/256.0
          CB= FLOAT(MOD( CLRFRC       ,256))/256.0
          CALL GSCR(WSID,I-1,CR,CG,CB)
C         WRITE(99,*) 'color:',I-1,'  fracs:',CR,CG,CB
        END IF
      IF (CLRFRC .GE. 0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   DOPLOT
     I                   (MESSFL)
C
C     + + + PURPOSE + + +
C     This routine calls PSETUP to open and activate the workstation,
C     calls PLTONE to make the plot, and calls PDNPLT to deactivate
C     and close the workstation.  This routine should only be called when
C     using the interactive AIDE routines.  For the PRIME with DISSPLA,
C     GKS is closed and opened because of bugs in that implementation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    RETCOD,IWAIT,SCLU,SGRP,ICLOS,WSID
C
C     + + + EXTERNALS + + +
      EXTERNAL   PSETUP, PLTONE, PRNTXT, PDNPLT
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 32
C
C     set up the plotting area, open workstation
      CALL PSETUP (RETCOD)
C
      IF (RETCOD .EQ. 0) THEN
C       make a plot
        CALL PLTONE
      ELSE
C       couldn't accept device
C       Could not get GKS or device, review ERROR.FIL
        SGRP = 140
        CALL PRNTXT (MESSFL, SCLU, SGRP)
      END IF
C
C     close work stations
      ICLOS = 1
      IWAIT = 1
      WSID = 1
      CALL PDNPLT (WSID,ICLOS,IWAIT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PDNPLT
     I                    (WSID, ICLOS,
     M                     IWAIT)
C
C     + + + PURPOSE + + +
C     This routine deactivates a workstation and closes a workstation
C     depending on the value of ICLOS.  It checks for computer type
C     (AIDE dependent) and does additional shutdowns necessary for
C     the PC and PRIME with DISSPLA.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  IWAIT, WSID, ICLOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WSID   - workstation id number (usually 1 unless multiple
C              Xwindows are used
C     ICLOS  - flag to close workstation.  Not used for PRIME or PC's
C                 0 - do not close the workstation
C                 1 - close the workstation
C     IWAIT  - set to 1 to require the user to press return before
C              plot device deactivated (and closed depending on ICLOS),
C              else set to 0.
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1, IC, IMET
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT, DNPLTW
C
C     + + + END SPECIFICATIONS + + +
C
C     find computer type
      I1 = 1
      CALL ANPRGT (I1, IC)
C
      IMET = 0
      IF (IC .EQ. 2 .AND. DEVTYP .GE. 4) IMET = 1
C
C     force IWAIT
      IF (IC .LE. 3 .AND. DEVTYP .EQ. 1) IWAIT = 1
C
      CALL DNPLTW (IWAIT,WSID,ICLOS,IC,IMET)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPOPEN
     O                    (FE)
C
C     + + + PURPOSE + + +
C     This routine calls the routine to open the GKS error file,
C     'error fil', and opens GKS.  It also calls routines to set
C     AIDE global variables.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FE     - Fortran unit number of GKS error file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4   APRMGP
C
C     + + + EXTERNALS + + +
      EXTERNAL   GBOPEN, GLVINI
C
C     + + + END SPECIFICATIONS + + +
C
C     call base gks open routine
      CALL GBOPEN
     O           (FE)
C
C     get global info on colors
      APRMGP= 120
      CALL GLVINI (APRMGP)
C     get global info on line types
      APRMGP= 121
      CALL GLVINI (APRMGP)
C     get global info on symbols
      APRMGP= 122
      CALL GLVINI (APRMGP)
C     get global info on fills
      APRMGP= 123
      CALL GLVINI (APRMGP)
C
      RETURN
      END

C
C
C
      SUBROUTINE   GSETMP
     I                    (XPHYS,YPHYS,XPAGE,YPAGE,DEVCOD,WINDOW,DRAWN,
     O                     XYOUT,NCOLI,RXDC,RYDC,INPT)
C
C     + + + PURPOSE + + +
C     This routine does the level one and two calls for GKS
C     to establish the plotting space, set the origin
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DEVCOD,NCOLI,INPT,WINDOW,DRAWN
      REAL      XPHYS,YPHYS,XPAGE,YPAGE,XYOUT,RXDC,RYDC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XPHYS  - world coordinate of x origin of plot
C     YPHYS  - world coordinate of y origin of plot
C     XPAGE  - length of plots x axis(wc)
C     YPAGE  - length of plots y axis(wc)
C     DEVCOD - device code(1-display,2-printer,3-plotter)
C     XYOUT  -
C     NCOLI  - number of colors available
C     RXDC  -
C     RYDC  -
C     INPT   - input device code
C     WINDOW - currently active map window
C     DRAWN  - flag indicating if the map is currently drawn in this window
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    ERR,VID,DCUNIT,FE,
     &             COLA,NPCI,ILEN,IPET,NSKD,NVLD,NCHD,NPCD,NSTD
      INTEGER      I1,I0,LX,LY,IVAL(1),IZAR(1)
      REAL         ZERO,X2,Y2,RXNDC,RYNDC,RTMP,RTMZ,
     1             RXMIN,RYMIN,XRPGE,YRPGE,RVAL(1),RXWIN,RYWIN
      CHARACTER*80 DATREC(1)
      CHARACTER*1  CVAL(1)
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL   GSWN, GSVP, GSELNT, GSWKWN, GSWKVP, GCLRWK 
      EXTERNAL   GQDSP, GQCF, GINLC, GQLI, DRWBDR, GSCLIP, GPREC
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
      I0    = 0
      FE    = 99
      ZERO  = 0.0
      VID   = 2
C
      IF (DRAWN.EQ.0) THEN
C       clear workstation
        CALL GCLRWK(WINDOW,I0)
      END IF
C     inquire about workstation
      CALL GQDSP (DEVCOD,
     O            ERR,DCUNIT,RXDC,RYDC,LX,LY)
      WRITE (FE,*) 'just inquired about workstation',DEVCOD,ERR
      WRITE (FE,*) '     units,sizes are ',DCUNIT,RXDC,RYDC,LX,LY
      IF (DEVCOD .EQ. 1100) THEN
C       AViiON X window
        RXWIN= 500.
      ELSE
C       other devices
        RXWIN= 800.
      END IF
      RYWIN= 600.
      IF (ERR .EQ. 0) THEN
C       determine normalized device coordinates
        IF (RXWIN .GT. RYWIN) THEN
          RXNDC = 1.0
          RYNDC = RYWIN/RXWIN
        ELSE
          RYNDC = 1.0
          RXNDC = RXWIN/RYWIN
        END IF
      ELSE
C       don't know size of device, give warning
        WRITE(FE,*) 'DONT KNOW ABOUT DEVICE, ERR:',ERR
      END IF
C
C     set global window (wc)
C     find limiting dimension
      RXMIN = RXWIN/XPAGE
      RYMIN = RYWIN/YPAGE
      WRITE(FE,*) ' X:',XPHYS,XPAGE,RXNDC,RXDC,RXWIN,RXMIN
      WRITE(FE,*) ' Y:',YPHYS,YPAGE,RYNDC,RYDC,RYWIN,RYMIN
      IF (RXMIN .LT. RYMIN) THEN
C       plot wider than screen
        XRPGE= XPAGE
        RTMP = RYWIN/RXWIN
        YRPGE= XPAGE*RTMP
        WRITE(FE,*) 'wider:',XRPGE,YRPGE,RTMP
      ELSE
C       plot higher than screen
        YRPGE= YPAGE
        RTMP = RXWIN/RYWIN
        XRPGE= YPAGE*RTMP
        WRITE(FE,*) 'higher:',XRPGE,YRPGE,RTMP
      END IF
      X2 = XRPGE+ XPHYS
      Y2 = YRPGE+ YPHYS
      CALL GSWN (VID,XPHYS,X2,YPHYS,Y2)
      WRITE (FE,*) 'just set global window (WC),X:',XRPGE,XPHYS,X2
      WRITE (FE,*) '                            Y:',YRPGE,YPHYS,Y2
C     set global viewport (ndc)
      CALL GSVP (VID,ZERO,RXNDC,ZERO,RYNDC)
      WRITE (FE,*) 'just set global viewport, (NDC)',RXNDC,RYNDC
C     select normalization transformation
      CALL GSELNT (VID)
      WRITE (FE,*) 'just selected normalization transformation',VID
C
      IF (RXNDC.LT.1.0) THEN
C       check x axis
        RTMP= XPAGE/YPAGE
        IF (RTMP.LT.RXNDC) THEN
          RTMZ = RXDC
          RXDC = RXDC* RTMP/RXNDC
          IF (RXDC.GT.RTMZ) RXDC= RTMZ
          RXNDC= RTMP
        END IF
C       check y axis
        RTMP= YPAGE/XPAGE
        IF (RTMP.GT.RXNDC .OR. RTMP.LT.RYNDC) THEN
          RTMZ = RYDC
          RYDC = RXDC* RTMP
          IF (RYDC.GT.RTMZ) RYDC= RTMZ
          RYNDC= RXNDC* RTMP
        END IF
      ELSE IF (RYNDC.LT.1.0) THEN
C       check y axis
        RTMP= YPAGE/XPAGE
        IF (RTMP.LT.RYNDC) THEN
          RTMZ = RYDC
          RYDC = RYDC* RTMP/RYNDC
          IF (RYDC.GT.RTMZ) RYDC= RTMZ
          RYNDC= RTMP
        END IF
C       check x axis
        RTMP= XPAGE/YPAGE
        IF (RTMP.GT.RYNDC .OR. RTMP.LT.RXNDC) THEN
          RTMZ = RXDC
          RXDC = RYDC* RTMP
          IF (RXDC.GT.RTMZ) RXDC= RTMZ
          RXNDC= RYNDC* RTMP
        END IF
      END IF
C
      XYOUT= XPAGE/RXNDC
      RTMZ = YPAGE/RYNDC
      WRITE (FE,*) 'XYOUT:',XYOUT,RTMZ
C     set workstation window (ndc)
      CALL GSWKWN (WINDOW,ZERO,RXNDC,ZERO,RYNDC)
      WRITE (FE,*) 'just set workstation window (NDC)', RXNDC,RYNDC
C     set workstation viewport
      CALL GSWKVP (WINDOW,ZERO,RXDC,ZERO,RYDC)
      WRITE (FE,*) 'just set workstation viewport (DC)',RXDC,RYDC
C
C     how many input devices available
      CALL GQLI (DEVCOD,
     O           ERR,INPT,NSKD,NVLD,NCHD,NPCD,NSTD)
      WRITE (FE,*) 'input device available:',DEVCOD,ERR,INPT,
     1  NSKD,NVLD,NCHD,NPCD,NSTD
C
      IF (ERR .EQ. 0) THEN
C       initialize locator at center of map
        IF (DEVCOD .EQ. 1100) THEN
C         AViiON X window
          IVAL(1)= I1
          IZAR(1)= 0
          CALL GPREC (I1,IVAL,I0,RVAL,I0,IZAR,CVAL,I1,
     O                ERR,ILEN,DATREC)
        ELSE
C         other devices
          ILEN= 1
          DATREC(1)= ' '
        END IF
        IPET= 1
        CALL GINLC (WINDOW,INPT,VID,XPHYS+XPAGE/2.,YPHYS+YPAGE/2.,IPET,
     1              ZERO,RXDC,ZERO,RYDC,ILEN,DATREC(1))
      ELSE
C       assume no input available
        INPT= 0
      END IF
C
C     colors
      CALL GQCF (DEVCOD,
     O           ERR,NCOLI,COLA,NPCI)
      WRITE(FE,*) 'just inquired color',ERR,NCOLI,COLA,NPCI
      IF (DEVCOD .NE. 1100) THEN
C       draw a border if not an X window
        CALL DRWBDR(FE,XPHYS,YPHYS,XPAGE,YPAGE)
      END IF
C     set clipping on
      WRITE (FE,*) 'SET CLIPPING ON'
      CALL GSCLIP(I1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DRWBDR
     I                   (FE,XPHYS,YPHYS,XPAGE,YPAGE)
C
C     draw a border at limits of dlg
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FE
      REAL      XPHYS,YPHYS,XPAGE,YPAGE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FE     - file unit to log errors, etc to
C     XPHYS  - world coordinate of x origin of plot
C     YPHYS  - world coordinate of y origin of plot
C     XPAGE  - length of plots x axis(wc)
C     YPAGE  - length of plots y axis(wc)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      REAL      X(5),Y(5),RLW
C
C     + + + EXTERNALS + + +
      EXTERNAL  GPL, GSLWSC, GSPLCI
C
C     + + + END SPECIFICATIONS + + +
C
      X(1)= XPHYS
      Y(1)= YPHYS
      X(2)= XPHYS
      Y(2)= YPHYS+ YPAGE*.9999
      X(3)= XPHYS+ XPAGE*.9999
      Y(3)= YPHYS+ YPAGE*.9999
      X(4)= XPHYS+ XPAGE*.9999
      Y(4)= YPHYS
      X(5)= XPHYS
      Y(5)= YPHYS
      WRITE (FE,*) 'border:     X  ',X
      WRITE (FE,*) '            Y  ',Y
C     border color is white
      CALL GSPLCI(1)
C     try thick line for border
      RLW = 2.0
      CALL GSLWSC(RLW)
      I   = 5
      CALL GPL(I,X,Y)
C     back to thin line
      RLW = 1.0
      CALL GSLWSC(RLW)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GELBLI
     I                   (ILABEL,MCLR,XYOUT,PX,PY)
C
C     + + + PURPOSE + + +
C     display an integer label on a map
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ILABEL,MCLR
      REAL      XYOUT,PX,PY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ILABEL - integer to display
C     MCLR   - color of label
C     XYOUT  -
C     PX     - x position to start label from
C     PY     - y position to start label from
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ILEN,JUST,OLEN
      REAL        RTMP
      CHARACTER*5 LABEL
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (LABEL,LABEL1)
      CHARACTER*1  LABEL1(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL    INTCHR, GSCHH, GSTXCI, GTX
C
C     + + + END SPECIFICATIONS + + +
C
C     make the integer a string
      ILEN= 5
      JUST= 1
      CALL INTCHR(ILABEL,ILEN,JUST,
     O            OLEN,LABEL1)
C
C     set character height
      RTMP= XYOUT*0.005
      CALL GSCHH(RTMP*2.0)
C     change color
      CALL GSTXCI(MCLR)
C     write id
      CALL GTX(PX+RTMP*1.5,PY-RTMP,LABEL(1:OLEN))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GELBLR
     I                   (RLABEL,MCLR,XYOUT,PX,PY)
C
C     + + + PURPOSE + + +
C     display an decimal label on a map
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MCLR
      REAL      RLABEL,XYOUT,PX,PY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RLABEL - decimal number to display
C     MCLR   - color of label
C     XYOUT  -
C     PX     - x position to start label from
C     PY     - y position to start label from
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ILEN,SIGDIG,DECPLA,OLEN
      REAL         RTMP
      CHARACTER*10 LABEL
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LABEL,LABL1)
      CHARACTER*1  LABL1(10)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    DECCHX, LFTSTR, LENSTR, GSCHH, GSTXCI, GTX
C
C     + + + END SPECIFICATIONS + + +
C
C     make the real number a string
      DECPLA= 2
      SIGDIG= 3
      ILEN  = 10
      CALL DECCHX(RLABEL,ILEN,SIGDIG,DECPLA,
     O            LABL1)
      CALL LFTSTR(ILEN,LABL1)
      OLEN= LENSTR(ILEN,LABL1)
C
C     set character height
      RTMP= XYOUT*0.005
      CALL GSCHH(RTMP*2.0)
C     change color
      CALL GSTXCI(MCLR)
C     write id
      CALL GTX(PX+RTMP*1.5,PY-RTMP,LABEL(1:OLEN))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GEMAGT
     I                   (INPT,DEVCOD,HANCNT,BRSFG,
     I                    RXDC,RYDC,XYOUT,XPHYS,YPHYS,
     I                    POPCNT,OPTID,OPTNAM,
     I                    STAFL,MKID,MKX,MKY,WINDOW,
     I                    NCLACT,CLACT,MSIZ,
     M                    MKDIS,MKSTAT,TXMIN,TXMAX,TYMIN,TYMAX,IANS,
     M                    ZOOMED,
     O                    CLICKD)
C
C     + + + PURPOSE + + +
C     let user get information about station displayed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     INPT,DEVCOD,HANCNT,IANS,WINDOW,ZOOMED,
     $            POPCNT,OPTID(POPCNT),STAFL,MKSTAT(*),
     $            CLICKD,NCLACT,CLACT(NCLACT),BRSFG
      REAL        RXDC,RYDC,XYOUT,XPHYS,YPHYS,MSIZ(NCLACT),
     $            MKID(*),MKX(*),MKY(*),MKDIS(*),
     $            TXMIN,TXMAX,TYMIN,TYMAX,PX,PY
      CHARACTER*5 OPTNAM(POPCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INPT  - input device code
C     DEVCOD- output device code
C     HANCNT- number of available handles found
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     RXDC  - x length in device coordinates
C     RYDC  - y length in device coordinates
C     XPHYS -
C     YPHYS - 
C     XYOUT -
C     POPCNT- number of active options
C     OPTID - array of option ids
C     OPTNAM- array of option names
C     STAFL - unit number of file containing details about station
C     MKID  - marker ID array
C     MKX   - marker X array
C     MKY   - marker Y array
C     MKDIS - marker distance array
C     MKSTAT- marker status array - 1-selected,2-unselected
C     TXMIN - x min values (wc)
C     TXMAX - x max values (wc)
C     TYMIN - y min values (wc)
C     TYMAX - y max values (wc)
C     IANS  - response from user
C             -2:draw again
C             -3:zoom
C             -4:full
C             -5:return
C     WINDOW- current map window
C     MSIZ  - marker size
C     ZOOMED- flag indicating if zoom has been used
C     CLICKD- did the user click on any points on the map
C     NCLACT- number of classes active
C     CLACT - active classes
C
C     + + + PARAMETERS + + +
      INTEGER   MXDSN
      PARAMETER (MXDSN=500)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cstaid.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,I,IT,IERR,IX,ITMP,FE,IOLD,ISTA,MTYP,
     1             STACLO,IPRNFL,DSPFG,VID,IPET,TMPLOC,MCLR1,MCLR2,
     2             IVAL(1),IZAR(1),J,LOCFLG,NDSN,ADSN(MXDSN),ACTFLG
      REAL         X1,X2,Y1,Y2,RTMP,ZERO,RVAL(1),MSIZF
      CHARACTER*80 DATREC(1)
      CHARACTER*1  CVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL     GRQLC, CALDIS, GINLC, STAPRT, GPREC, GEDOPT
      EXTERNAL     GSCHH, GSTXCI, GTX, GMARK1, GSPMCI, CLGDSN
      EXTERNAL     TSDSLC, CLGETG, TSDSAC
C
C     + + + END SPECIFICATIONS + + +
C
      CLICKD= 0
      I0  = 0
      I1  = 1
      VID = 2
      FE  = 99
      ZERO= 0.
C     open output printer
      IPRNFL= 97
      OPEN (UNIT=IPRNFL,FILE='PRT')
C     no station at start
      ISTA= 0
 10   CONTINUE
C       let user get more info, sample a point
        CALL GRQLC (WINDOW,INPT,IX,IT,X1,Y1)
C       WRITE (FE,*) 'location selected:',IT,X1,Y1
        X1= XPHYS+ X1*XYOUT
        Y1= YPHYS+ Y1*XYOUT
C       WRITE (FE,*) 'wc location:',X1,Y1
C       get location of nearest handle
        CALL CALDIS (X1,Y1,HANCNT,MKX,MKY,
     O               MKDIS,STACLO)
C       WRITE (FE,*) 'STACLO:',STACLO
C       WRITE (FE,*) 'XY:    ',MKX(STACLO),MKY(STACLO)
        IF (DEVCOD .EQ. 1100) THEN
C         AViiON X - pack color index for locator
          IVAL(1)= I1
          IZAR(1)= 0
          CALL GPREC (I1,IVAL,I0,RVAL,I0,IZAR,CVAL,I1,
     O                IERR,ITMP,DATREC)
        ELSE
          ITMP     = 1
          DATREC(1)= ' '
        END IF
        WRITE(FE,*) 'IERR,ETC',IERR,ITMP,ZERO,RXDC,RYDC
C       save last station id
        IOLD= ISTA
        IPET= 1
        CALL GINLC (WINDOW,INPT,VID,MKX(STACLO),MKY(STACLO),
     1              IPET,ZERO,RXDC,ZERO,RYDC,ITMP,DATREC(1))
        ISTA= MKID(STACLO)
        WRITE(FE,*) 'ID:     ',ISTA,IANS
        IF (ISTA .GT. 0) THEN
C         a valid station id
          IF (IANS.EQ.-1) THEN
C           identify station selected
            WRITE(FE,*) 'station record:',ISTA
            IF (DEVCOD .EQ. 1100) THEN
C             display a record to X window
              DSPFG= 2
            ELSE
C             only one window, single line above options
              DSPFG= 0
C             x and y positions
              PY   = TYMIN+ XYOUT* .04
              PX   = TXMIN+ XYOUT* .005
C             set character height
              CALL GSCHH(XYOUT*.012)
              IF (IOLD.GT.0) THEN
C               erase prev, set color to black
                CALL GSTXCI(I0)
C               write station name
                IF (STANAM(IOLD).NE.' ') THEN
                  CALL GTX(PX,PY,STANAM(IOLD))
                ELSE
                  CALL GTX(PX,PY,LOCID(IOLD))
                END IF
              END IF
C             set color to white
              CALL GSTXCI(I1)
C             write station name
              IF (STANAM(ISTA).NE.' ') THEN
                CALL GTX(PX,PY,STANAM(ISTA))
              ELSE
                CALL GTX(PX,PY,LOCID(ISTA))
              END IF
            END IF
            IF (DSPFG .GE. 0) THEN
C             this routine supplied by the application program
              CALL STAPRT(IPRNFL,DSPFG,STAFL,ISTA,BRSFG,
     I                    XYOUT,TYMIN,TXMIN,
     I                    POPCNT,OPTID,OPTNAM,IANS,DEVCOD,
     M                    HANCNT,MKID,MKX,MKY)
            END IF
          END IF
C
          IF (IANS.EQ.-1) THEN
C           select/unselect
            IF (BRSFG.EQ.1) THEN
C             user browsed, see if any data sets selected
              DO 21 J = 1,NCLACT
C               check each active class to see if location here
                CALL CLGDSN (CLACT(J),MXDSN,I0,
     O                       NDSN,ADSN)
                LOCFLG = 0
                MKSTAT(ISTA) = 2
                DO 31 I= 1,NDSN
C                 get location associated with each data set in class
                  CALL TSDSLC (ADSN(I),
     O                         TMPLOC)
                  IF (TMPLOC.EQ.ISTA) THEN
C                   this class is at this location, flag it
                    LOCFLG = 1
C                   see if selected
                    CALL TSDSAC (ADSN(I),
     O                           ACTFLG)
                    IF (ACTFLG.EQ.1) THEN
                      MKSTAT(ISTA) = 1
                    END IF
                  END IF
 31             CONTINUE
                IF (LOCFLG.EQ.1) THEN
C                 set polymarker color in gks
                  CALL CLGETG (CLACT(J),
     O                         MTYP,MCLR2,MCLR1,MSIZF)
                  IF (MKSTAT(ISTA).EQ.1) THEN
                    CALL GSPMCI (MCLR1)
                  ELSE
                    CALL GSPMCI (MCLR2)
                  END IF
                  CALL GMARK1 (MTYP,MSIZ(J),MKX(STACLO),MKY(STACLO))
                  CLICKD = 1
                END IF
 21           CONTINUE
            ELSE
C             automatically select/unselect all data sets here
              IF (MKSTAT(ISTA).EQ.2) THEN
C               status is 'unselected', change to 'selected'
                DO 20 J = 1,NCLACT
C                 check each active class to see if location here
                  CALL CLGDSN (CLACT(J),MXDSN,I0,
     O                         NDSN,ADSN)
                  LOCFLG = 0
                  DO 30 I= 1,NDSN
C                   get location associated with each data set in class
                    CALL TSDSLC (ADSN(I),
     O                           TMPLOC)
                    IF (TMPLOC.EQ.ISTA) THEN
C                     this class is at this location, flag it
                      LOCFLG = 1
                    END IF
 30               CONTINUE
                  IF (LOCFLG.EQ.1) THEN
C                   set polymarker color in gks
                    CALL CLGETG (CLACT(J),
     O                           MTYP,MCLR2,MCLR1,MSIZF)
                    CALL GSPMCI (MCLR1)
                    CALL GMARK1 (MTYP,MSIZ(J),MKX(STACLO),MKY(STACLO))
                    MKSTAT(ISTA)= 1
                    CLICKD = 1
                  END IF
 20             CONTINUE
              ELSE IF (MKSTAT(ISTA).EQ.1) THEN
C               status is 'selected', change to 'unselected'
                DO 25 J = 1,NCLACT
C                 check each active class to see if location here
                  CALL CLGDSN (CLACT(J),MXDSN,I0,
     O                         NDSN,ADSN)
                  LOCFLG = 0
                  DO 35 I= 1,NDSN
C                   get location associated with each data set in class
                    CALL TSDSLC (ADSN(I),
     O                           TMPLOC)
                    IF (TMPLOC.EQ.ISTA) THEN
C                     this class is at this location, flag it
                      LOCFLG = 1
                    END IF
 35               CONTINUE
                  IF (LOCFLG.EQ.1) THEN
C                   set polymarker color in gks
                    CALL CLGETG (CLACT(J),
     O                           MTYP,MCLR2,MCLR1,MSIZF)
                    CALL GSPMCI (MCLR2)
                    CALL GMARK1 (MTYP,MSIZ(J),MKX(STACLO),MKY(STACLO))
                    MKSTAT(ISTA)= 2
                    CLICKD = 1
                  END IF
 25             CONTINUE
              END IF
            END IF
          END IF
        ELSE
C         an option
          IANS= ISTA
          IF (IANS .NE. -5) THEN
C           display option active
            CALL GEDOPT (I0,XYOUT,TYMIN,TXMIN,
     I                   POPCNT,OPTID,OPTNAM,IANS,DEVCOD,
     M                   HANCNT,MKID,MKX,MKY)
          END IF
          IF (IANS .EQ. -4) THEN
C           since window is back to full there's no need to remember
C           that zoom was used
            ZOOMED = 0
          END IF
          IF (IANS .EQ. -3) THEN
C           zoom, change flag so map will be redrawn next time
            ZOOMED = 1
C           sample first point
            CALL GRQLC (WINDOW,INPT,IX,IT,X1,Y1)
            WRITE (FE,*) 'zoom1 location selected:',IT,X1,Y1
            X1= XPHYS+ X1*XYOUT
            Y1= YPHYS+ Y1*XYOUT
            WRITE(FE,*) 'zoom, pt1:',IERR,ITMP,X1,Y1
C           move to first point
            IF (DEVCOD .EQ. 1100) THEN
C             AViiON X - pack color index for locator
              IVAL(1)= I1
              IZAR(1)= 0
              CALL GPREC (I1,IVAL,I0,RVAL,I0,IZAR,CVAL,I1,
     O                    IERR,ITMP,DATREC)
              WRITE(FE,*) IERR,ITMP
              IPET= 1
            ELSE
C             others, rect pet
              IPET= 1
              ITMP= 1
              DATREC(1)= ' '
            END IF
            WRITE(FE,*) IPET,ZERO,RXDC,RYDC
            CALL GINLC (WINDOW,INPT,VID,X1,Y1,
     1                  IPET,ZERO,RXDC,ZERO,RYDC,ITMP,DATREC(1))
C           sample second point
            CALL GRQLC (WINDOW,INPT,IX,IT,X2,Y2)
            WRITE (FE,*) 'zoom2 location selected:',IT,X2,Y2
            X2= XPHYS+ X2*XYOUT
            Y2= YPHYS+ Y2*XYOUT
C           check no abort
            CALL CALDIS (X2,Y2,HANCNT,MKX,MKY,
     O                   MKDIS,STACLO)
            I   = MKID(STACLO)
            RTMP= MKDIS(HANCNT)/XYOUT
            WRITE(FE,*) 'zoom 2nd pt:',X2,Y2,RTMP,STACLO
            IF (I .EQ. -5 .AND. RTMP .LT. 0.03) THEN
C             user selected return, get out of zoom
C             reset to identify
              IANS= -1
C             display option active
              CALL GEDOPT (I0,XYOUT,TYMIN,TXMIN,
     I                     POPCNT,OPTID,OPTNAM,IANS,DEVCOD,
     M                     HANCNT,MKID,MKX,MKY)
            ELSE
C             reset coords for zoom
              IF (X1 .GT. X2) THEN
C               first pt bigger x
                TXMIN= X2
                TXMAX= X1
              ELSE
C               second pt bigger x
                TXMIN= X1
                TXMAX= X2
              END IF
              IF (Y1 .GT. Y2) THEN
C               first pt bigger y
                TYMIN= Y2
                TYMAX= Y1
              ELSE
C               second pt bigger y
                TYMIN= Y1
                TYMAX= Y2
              END IF
              WRITE(FE,*) 'zoom to:',TXMIN,TXMAX,TYMIN,TYMAX
            END IF
          END IF
        END IF
        WRITE(FE,*) 'IANS:   ',IANS
      IF (IANS .GE. -1) GOTO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   GEDOPT
     I                    (INITFG,XYOUT,TYMIN,TXMIN,
     I                     POPCNT,OPTID,OPTNAM,IANS,DEVCOD,
     M                     HANCNT,MKID,MKX,MKY)
C
C     + + + PURPOSE + + +
C     display valid options on map and save centroids
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     INITFG,POPCNT,OPTID(POPCNT),IANS,HANCNT,
     $            DEVCOD
      REAL        XYOUT,TYMIN,TXMIN,MKID(*),MKX(*),MKY(*)
      CHARACTER*5 OPTNAM(POPCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INITFG - initialize flag, 1-save locations, 0-dont save locations
C     XYOUT  -
C     TYMIN  - y min pos(WC)
C     TXMIN  - x min pos(WC)
C     POPCNT - number of active options
C     OPTID  - array of option ids
C     OPTNAM - array of option names
C     IANS   - current option, zero to erase options
C     HANCNT - count of handles in use
C     MKID   - marker ID
C     MKX    - marker X coord
C     MKY    - marker Y coord
C     DEVCOD - device code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ICOL,N,SBCOLR,UBCOLR,SFCOLR,UFCOLR
      REAL      PXINC,PX,PY,BX(4),BY(4),LSIZE
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTX,GSCHH,GSTXCI,GFA,GSFACI,GSFAIS,GSFASI,MOPGET
C
C     + + + END SPECIFICATIONS + + +
C
C     y position
      PY   = TYMIN+ XYOUT* .010
      IF (DEVCOD.NE.1100) THEN
C       not x window
        PY   = TYMIN+ XYOUT* .015
      END IF
      PXINC= 0.07* XYOUT
C
C     get map option label specs
      CALL MOPGET (
     O             LSIZE,SBCOLR,UBCOLR,SFCOLR,UFCOLR)
C     set character height
      CALL GSCHH(XYOUT*LSIZE)
      DO 10 I= 1,POPCNT
C
C       draw background
C       x position
        IF (I.EQ.1) THEN
C         leave room for left border
          BX(1)= TXMIN+(0.003*XYOUT)
          BX(4)= TXMIN+(0.003*XYOUT)
        ELSE
          BX(1)= TXMIN+ ((I-1)* PXINC* LSIZE/.01)
          BX(4)= TXMIN+ ((I-1)* PXINC* LSIZE/.01)
        END IF
        BX(2)= TXMIN + ((I)* (PXINC)* LSIZE/.01)
        BX(3)= TXMIN + ((I)* (PXINC)* LSIZE/.01)
C       leave some room between boxes
        BX(2)= BX(2) - (0.05*(BX(2)-BX(1)))
        BX(3)= BX(3) - (0.05*(BX(3)-BX(1)))
C       y position
        BY(1) = TYMIN+(0.005*XYOUT)
        BY(2) = TYMIN+(0.005*XYOUT)
        BY(3) = TYMIN+(2.5*LSIZE*XYOUT)
        BY(4) = TYMIN+(2.5*LSIZE*XYOUT)
C       set color
        IF (IANS .EQ. 0) THEN
C         turn off options
          ICOL= 0
        ELSE IF (IANS .EQ. OPTID(I)) THEN
C         current option
          ICOL= SBCOLR
        ELSE
C         other options yellow
          ICOL= UBCOLR
        END IF
C       change color
        CALL GSFACI(ICOL)
C       set fill area style
        N = -2
        IF (DEVCOD.NE.1100) THEN
C         not x window
          N = 1
        END IF
        CALL GSFAIS(N)
        N = -2
        CALL GSFASI(N)
C       make box
        N = 4
        CALL GFA(N,BX,BY)
C
C       write text
C       x position
        PX= TXMIN+ (PXINC*0.1)+ ((I-1)* PXINC* LSIZE/.01)
C       set color
        IF (IANS .EQ. 0) THEN
C         turn off options
          ICOL= 0
        ELSE IF (IANS .EQ. OPTID(I)) THEN
C         current option
          ICOL= SFCOLR
        ELSE
C         other options yellow
          ICOL= UFCOLR
        END IF
C       change color
        CALL GSTXCI(ICOL)
C       write id
        CALL GTX(PX,PY,OPTNAM(I))
        IF (INITFG .EQ. 1) THEN
C         save locations
          HANCNT= HANCNT+ 1
          MKID(HANCNT)= OPTID(I)
          MKX(HANCNT) = PX+ PXINC/2.0
          MKY(HANCNT) = PY
C         WRITE(99,*) 'handle:',HANCNT,OPTID(I),MKX(HANCNT),PY
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   MSETUP
     I                   (DEVTYP,DEVCOD,WINDOW,WINDIM,
     M                    MSIZF,
     O                    RETCOD,IWAIT,IMET)
C
C     + + + PURPOSE + + +
C     *** taken from PSETUP/GSETUP, specific for mapping graphics ***
C     This routine does the level one and two calls for GKS
C     to set the scales, establish the plotting space, set the
C     origin, and allow the axes to be labeled by special routines.
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DEVTYP, DEVCOD, RETCOD, IWAIT, IMET, WINDOW
      REAL       WINDIM(4,7),MSIZF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DEVTYP - device type
C     DEVCOD - device code
C     RETCOD - return code    0 = everything ok,
C                             1 = device not setup to plot
C     IWAIT  - flag for waiting after plot is done
C              0 - don't wait
C              1 - wait for user to use any key to continue
C     IMET   - 0 - no action required
C              1 - DISSPLA meta file so change name
C     WINDOW - current map window
C     WINDIM - array of window dimensions and locations
C     MSIZF  - marker size factor
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ERR,ALH,ALV,ISTATE,LBCOLR,
     &          IERR,I1(13),ISS,I
      INTEGER   L0, CONID, ACONID(5), BCOLOR, L1, FCOLOR
      INTEGER   IA(10),LSTR(5),ERRIND,LDR,LODR
      INTEGER   IC, PREC, FONT, CLRFRC
      INTEGER   PRMIND, JR, IBLUE, IGREEN, IRED, ICHSP, ICHXP
      REAL      RA(1)
      REAL      CHH,CHBX,CHBY
      REAL      CHSP, CHXP, BCR, BCG, BCB
      REAL      RED(2), GREEN(2), BLUE(2)
      REAL      CR,CG,CB, FCR,FCG,FCB
      CHARACTER*8 CLRSTR
      CHARACTER*80 DATREC(10), ODR(10), STR
C
C     + + + INTRINSICS + + +
      INTRINSIC  INT, REAL, CHAR, FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT, GQTXFP, GSCHXP, GQCHXP
      EXTERNAL   GQCHH, GQCHSP, GQCHB, GSASF, GPLBCL
      EXTERNAL   GSCHSP, GSTXAL, GOPWK, GACWK, GQWKS
      EXTERNAL   GSTXFP, GQOPS, GSCR
      EXTERNAL   GPREC, GESC, GGTGLA, GGMTFL
C
C     + + + DATA INITIALIZATIONS + + +
C                                  pc   prime  vax  unix  aviion
      DATA  I1/13*1/, L0/0/, ACONID/0,   10,   10,   0,   -32768/
      DATA  RED,GREEN,BLUE/0.0,1.0,0.0,1.0,0.0,1.0/, L1/1/
C
C     + + + END SPECIFICATIONS + + +
C
C     *** following code (to start of GSETUP code) taken from PSETUP ***
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
        PRMIND = 48
      ELSE IF (DEVTYP .EQ. 2) THEN
C       printer
        PRMIND = 49
      ELSE
C       screen
        PRMIND = 50
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
        BCR = RED(BCOLOR)
        BCG = GREEN(BCOLOR)
        BCB = BLUE(BCOLOR)
      ELSE
C       input from TERM.DAT file
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
      MSIZF = MSIZF* (REAL(JR)/100.0)
C
C
C     ***   following code (to start of color table code) taken from GSETUP
C
      IWAIT = 0
      IERR  = 0
      RETCOD= 0
      CONID = ACONID(IC)
C
      WRITE (99,*)
      WRITE (99,*) '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
C     find state of system
      CALL GQOPS (ISS)
      WRITE(99,*) 'State of system is',ISS,IC,DEVTYP,DEVCOD
C
C     make all text attributes INDIVIDUAL
      CALL GSASF (I1)
C     initialize the GKS environment
      IF (IC .EQ. 5) THEN
C       computer is Aviion with Prior GKS
        IF (DEVCOD .EQ. 1100 .OR. DEVCOD .EQ. 1101) THEN
C         X window size
          IA(1) = 1
          IA(2) = 0
          IA(3) = INT(WINDIM(1,WINDOW)*1271.0)
          IA(4) = INT(WINDIM(2,WINDOW)*996.0)
          IA(5) = INT(WINDIM(3,WINDOW)*1271.0)
          IA(6) = INT(WINDIM(4,WINDOW)*996.0)
          STR   = 'GKS WINDOW '//CHAR(WINDOW+48)
          LSTR(1)= 14
          CALL GPREC(6,IA,0,RA,1,LSTR,STR,12,ERRIND,LDR,DATREC)
          CALL GESC(-9,LDR,DATREC,10,LODR,ODR)
        ELSE
C         not X window, must need a file, so set unit number
          CALL GGMTFL(CONID)
        END IF
      END IF
C
C     inquire about workstation
      CALL GQWKS(WINDOW,
     O           IERR,ISTATE)
      WRITE(99,*) 'WORKSTATION INQUIRE:',WINDOW,IERR,ISTATE
      IF (IERR .NE. 0) THEN
C       open specific workstation
        WRITE (99,*) 'ABOUT TO OPEN DEVICE',DEVCOD
        CALL GOPWK (WINDOW,CONID,DEVCOD)
C       color table (values in table start with index of 0)
        CLRSTR= 'COLOR   '
        I= 0
 10     CONTINUE
          I= I+ 1
          CALL GGTGLA (CLRSTR,I,
     O                 CLRFRC)
          IF (CLRFRC .GT. 0) THEN
C           update color fraction
            CR= FLOAT     (CLRFRC/65536)      /256.0
            CG= FLOAT(MOD((CLRFRC/256  ),256))/256.0
            CB= FLOAT(MOD( CLRFRC       ,256))/256.0
            CALL GSCR(WINDOW,I-1,CR,CG,CB)
C           WRITE(99,*) 'color:',I-1,'  fracs:',CR,CG,CB
          END IF
        IF (CLRFRC .GE. 0) GO TO 10
      END IF
C
C     Check for meta file for Prime
      IF (IMET .EQ. 0 .OR. IC .NE. 2) THEN
C       not a meta file, ok to activate
        WRITE (99,*) 'ACTIVATE WORKSTATION'
        CALL GACWK (WINDOW)
        WRITE (99,*) 'GET WORKSTATION CHARACTERISTICS'
        CALL GQWKS (WINDOW,
     O              IERR,ISTATE)
        WRITE (99,*) 'WORKSTATION STATE & ERROR CODE',ISTATE,IERR
      ELSE
        ISTATE= 1
      END IF
C
C     set IWAIT based on device type
      IF (DEVTYP .EQ. 1) THEN
C       output to screen
        IWAIT = 1
      END IF
C
      IF (ISTATE.EQ.0 .OR. IERR .NE. 0) THEN
C       device & not available, can't make plot
        WRITE(99,*) 'DEVICE',DEVCOD,' NOT AVAILABLE'
        RETCOD = 1
      ELSE
C       set text font and precision
        CALL GSTXFP (FONT, PREC)
        CALL GQTXFP (ERR,FONT,PREC)
        WRITE (99,*) 'FONT, PRECISION, ERROR CODE ARE ',FONT,PREC,ERR
C
C       set expansion factor
        CALL GSCHXP (CHXP)
        CALL GQCHXP (ERR,CHXP)
        WRITE (99,*) 'EXPANSION FACTOR AND ERROR CODE',CHXP,ERR
C
        CALL GSCHSP (CHSP)
        CALL GQCHSP (ERR,CHSP)
        WRITE (99,*) 'CHARACTER SPACING AND ERROR CODE',CHSP,ERR
C
        ALH = 0
        ALV = 0
C       write(*,*) 'vertical text alignment (1-6)'
C       read(*,*) alv
C       write(*,*) 'horizontal text alignment(1-4)'
C       read(*,*) alh
        CALL GSTXAL (ALH, ALV)
        WRITE (99,*) 'TEXT ALIGNMENT ',ALH,ALV
C
        CALL GQCHH (ERR,CHH)
        WRITE (99,*) 'CHARACTER HEIGHT (WC) AND ERROR CODE',CHH,ERR
        CALL GQCHB (ERR,CHBX,CHBY)
        WRITE (99,*) 'BASE VECTOR AND ERROR CODE',CHBX,CHBY,ERR
C
C       set background color
        CALL GSCR (WINDOW,L0,BCR,BCG,BCB)
      END IF
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
      CALL GSCR(WINDOW,L1,FCR,FCG,FCB)
C     set LBCOLR to color index 1
      LBCOLR = 1
      CALL GPLBCL(LBCOLR)
C    
      CLRSTR= 'COLOR   '
      I= 0
 20   CONTINUE
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
          CALL GSCR(WINDOW,I-1,CR,CG,CB)
C         WRITE(99,*) 'color:',I-1,'  fracs:',CR,CG,CB
        END IF
      IF (CLRFRC .GE. 0) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETITL
     I                   (BASNAM,ICOL,LSIZE,XYOUT,TYPHYS,TYPAGE,
     I                    XPAGE,XPHYS)
C
C     + + + PURPOSE + + +
C     display basin name as title of map
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ICOL
      REAL         XYOUT,TYPHYS,TYPAGE,XPAGE,XPHYS,LSIZE
      CHARACTER*64 BASNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XYOUT  -
C     BASNAM - basin name to use as title of map
C     TYPHYS -
C     TYPAGE -
C     XPHYS  - world coordinate of x origin of plot
C     XPAGE  - length of plots x axis(wc)
C     ICOL   - color of map title
C     LSIZE  - size of map title
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,I64
      REAL      PX,PY
      CHARACTER*1   BASNM1(64)
      CHARACTER*64  CTITLE
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTX,GSCHH,GSTXCI,GSTXAL,CVARAR,CARVAR,CTRSTR
C
C     + + + END SPECIFICATIONS + + +
C
C     y position
      PY = TYPHYS + (TYPAGE*.97)
C     PY = TYMIN + (XYOUT*.97)
C     x position
      PX = (XPAGE/2) + XPHYS
C     set character height
      CALL GSCHH(XYOUT*LSIZE)
C     set color
      CALL GSTXCI(ICOL)
C     set to center text
      I = 2
      J = 0
      CALL GSTXAL (I,J)
C     center text in string
      I64 = 64
      CALL CVARAR (I64,BASNAM,I64,BASNM1)
      CALL CTRSTR (I64,BASNM1)
      CALL CARVAR (I64,BASNM1,I64,CTITLE)
C     write title
      CALL GTX (PX,PY,CTITLE)
C     set to left justify text
      I = 0
      CALL GSTXAL (I,I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GELEGD
     I                   (XYOUT,TYMIN,TYMAX,XPAGE,XPHYS,BMSIZE,
     I                    BASEY,BASEX,LSIZE,ICOL)
C
C     + + + PURPOSE + + +
C     display map legend
C
C     + + + DUMMY ARGUMENTS + + +
      REAL         XYOUT,TYMIN,XPAGE,XPHYS,TYMAX,BMSIZE,BASEX,BASEY,
     1             LSIZE
      INTEGER      ICOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XYOUT  -
C     TYMIN  - y min pos(WC)
C     TYMAX  - y min pos(WC)
C     XPHYS  - world coordinate of x origin of plot
C     XPAGE  - length of plots x axis(wc)
C     BMSIZE - marker size factor
C     BASEX  - base position of legend in horiz
C     BASEY  - base position of legend in vert
C     ICOL   - color of legend text
C     LSIZE  - lettering size
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        NCLAS,MAPMRK(3),IACT,I
      REAL           PY,PX,MSIZ,MSIZF
      CHARACTER*8    CSCEN,CLOC,CCONS
      CHARACTER*64   LEGNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTX,GSCHH,GSTXCI,GSTXAL
      EXTERNAL  CLCNT,CLGET,GSPMCI,GMARK1,CLGETG
C
C     + + + END SPECIFICATIONS + + +
C
C     do legend title
C     y position
      PY = TYMIN + (XYOUT*BASEY)
C     x position
      PX = (XPAGE*BASEX) + XPHYS
C     set character height
      CALL GSCHH(XYOUT*LSIZE)
C     set color
      CALL GSTXCI(ICOL)
C     set legend title
      LEGNAM = 'LEGEND'
C     set to left justify text
      I = 0
      CALL GSTXAL (I,I)
C     write title
      CALL GTX (PX,PY,LEGNAM)
C
C     y position
      BASEY = BASEY - .025
      PY = TYMIN + (XYOUT*BASEY)
C     set to left justify text
      I = 0
      CALL GSTXAL (I,I)
C     set legend title
      LEGNAM = 'Sel'
      CALL GTX (PX,PY,LEGNAM)
C     x position
      PX = (XPAGE*(BASEX+.08)) + XPHYS
      LEGNAM = 'Unsel'
      CALL GTX (PX,PY,LEGNAM)
C     x position
      PX = (XPAGE*(BASEX+.22)) + XPHYS
      LEGNAM = 'Scen'
      CALL GTX (PX,PY,LEGNAM)
C     x position
      PX = (XPAGE*(BASEX+.42)) + XPHYS
      LEGNAM = 'Const'
      CALL GTX (PX,PY,LEGNAM)
C
C     what classes are active?
      CALL CLCNT(NCLAS)
      DO 10 I= 1,NCLAS
        CALL CLGET(I,CSCEN,CLOC,CCONS,IACT)
        IF (IACT.EQ.1) THEN
C         this class is active
          IF (CSCEN.EQ.'        ') THEN
C           set scen to all
            CSCEN = '<all>'
          END IF
          IF (CCONS.EQ.'        ') THEN
C           set cons to all
            CCONS = '<all>'
          END IF
          CALL CLGETG (I,
     O                 MAPMRK(1),MAPMRK(3),MAPMRK(2),MSIZF)
C         marker size (world coords)
          MSIZ = (TYMAX-TYMIN)*MSIZF*BMSIZE/50
C         y position
          BASEY = BASEY - .025
          PY = TYMIN + (XYOUT*BASEY)
C         x position
          PX = (XPAGE*(BASEX+.02)) + XPHYS
C         set polymarker color in gks
          CALL GSPMCI (MAPMRK(2))
C         display select marker
          CALL GMARK1 (MAPMRK(1),MSIZ,PX,PY)
C         x position
          PX = (XPAGE*(BASEX+.10)) + XPHYS
C         set polymarker color in gks
          CALL GSPMCI (MAPMRK(3))
C         display unselect marker
          CALL GMARK1 (MAPMRK(1),MSIZ,PX,PY)
C         set legend text
          LEGNAM= CSCEN
C         x position
          PX = (XPAGE*(BASEX+.22)) + XPHYS
C         write title
          CALL GTX (PX,PY,LEGNAM)
C         set legend text
          LEGNAM= CCONS
C         x position
          PX = (XPAGE*(BASEX+.42)) + XPHYS
C         write title
          CALL GTX (PX,PY,LEGNAM)
        END IF
 10   CONTINUE
C
      RETURN
      END

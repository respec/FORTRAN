C
C
C
      SUBROUTINE   GBOPEN
     O                    (FE)
C
C     + + + PURPOSE + + +
C     This routine opens the GKS error file, 'error fil', and
C     opens GKS.  If the error.fil for AIDE has been opened, that
C     file is used instead of a new file.  Special subroutine calls
C     for the PC implementation of GKS are also made.  For other
C     systems, these are dummy subroutines.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FE     - Fortran unit number of GKS error file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cgracm.inc'
      INCLUDE 'cgrapm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4   GRATMP
      LOGICAL*4   LTF
C
C     + + + EXTERNALS + + +
      EXTERNAL   GOPKS, GPERFL, PCGRGT, PCGRST
C
C     + + + END SPECIFICATIONS + + +
C
C     open the error file, if not already opened by AIDE routines
      FE = 99
      LTF = .FALSE.
      INQUIRE (UNIT=FE, OPENED=LTF)
      IF (LTF .EQV. .FALSE.) THEN
C       file not already open
        OPEN (UNIT=FE, FILE='ERROR.FIL')
      END IF
C
C     put unit number in common block
      CALL GPERFL (FE)
C
C     get current graphics page/mode for PC
C     PCGRGT ig null for other systems
      CALL PCGRGT (TPAGE, TMODE)
C
C     open GKS
      GRASZE = GRADIM
      GRATMP = GRASZE
      CALL GOPKS (FE, GRATMP)
      WRITE(FE,*) 'OPEN GKS'
C
C     restore above graphics page/mode for PC
C     PCGRST is null for other systems
      CALL PCGRST (TPAGE,TMODE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPCLOS
     I                    (FE)
C
C     + + + PURPOSE + + +
C     This routine closes the GKS error file and closes GKS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FE     - Fortran unit number for GKS error file
C
C     + + + EXTERNALS + + +
      EXTERNAL   GCLKS
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(FE,*) 'CLOSE GKS'
C     CLOSE (FE,ERR=10)
C10   CONTINUE
C
      CALL GCLKS
C
      RETURN
      END
C
C
C
      SUBROUTINE   DNPLTW
     I                    (IWAIT,WSID,ICLOS,IC,IMET)
C
C     + + + PURPOSE + + +
C     This routine deactivates and closes the workstation.  For PC's
C     it resets the graphics mode with a subroutine that does nothing
C     for other systems.  If IWAIT has a value of 1, the routine waits
C     for the return(enter) key to be pressed by the user.  This
C     routine does not need to be called when using the AIDE routines
C     and DOPLOT.  It is the same as DONPLT, except the workstation
C     ID is specified and not defaulted to 1.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IWAIT, WSID, ICLOS, IC, IMET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IWAIT  - flag for waiting for a response from the user
C              after plot is done
C              0 - don't wait
C              1 - wait for computer types pc and prime
C                  used with aide user interaction
C              2 - force wait no matter what computer type
C                  used with programs not using aide
C     WSID   - workstation id number assigned by the programmer
C     ICLOS  - flag to close the workstation
C              0 - keep open
C              1 - close
C     IC     - computer type (1=pc,2=prime,3=vax,4=sun,5=dg aviion)
C     IMET   - flag for DISSPLA meta files on PRIME
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cgrapm.inc'
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RETCOD, HV, LEN, STATE
      REAL         XSIZ, SIZL, XPOS, YPOS
      CHARACTER*1  C1, ASTR(48)
      CHARACTER*48 STR
C
C     + + + FUNCTIONS + + +
      REAL   ANGLEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   GDAWK, GCLWK, PCGRST, GBOPEN,  PRMETA
      EXTERNAL   GCLKS, CVARAR, ANGTXA, ANGLEN, GSCHH, GQWKS
      EXTERNAL   IGrPause
C
C     + + + END SPECIFICATIONS + + +
C
C     whats the state of things?
      CALL GQWKS(WSID,
     O           RETCOD,STATE)
C
      IF (RETCOD .EQ. 0) THEN
C       workstation must be open
        IF (IWAIT .EQ. 2) THEN
C         force wait if graphics window
          IF (DEVTYP .EQ. 1) THEN
C           write message on top of plot
            LEN = 48
            SIZL = SIZEL*1.5
            CALL GSCHH (SIZL)
            STR = 'Press Return (Enter) when finished viewing plot.'
            CALL CVARAR (LEN,STR,LEN,ASTR)
            XSIZ = ANGLEN (LEN,ASTR)
            XPOS = 0.5*(XLEN - XSIZ)
            IF (XPOS .LT. 0) THEN
C             shrink letter size to fit
              SIZL = SIZL*XLEN/XSIZ
              CALL GSCHH (SIZL)
              XPOS = 0.0
            END IF
            YPOS = YLEN + SIZEL
            HV = 1
            CALL ANGTXA (XPOS,YPOS,LEN,ASTR,HV)
            CALL GSCHH (SIZEL)
C           wait for user to hit any key to continue
            !READ (*,FMT='(A1)') C1
            CALL IGrPause('AP')
          END IF
        ELSE IF (IWAIT .EQ. 1) THEN
C         wait for pc and prime
          IF (IC .LE. 2) THEN
C           write message on top of plot
            LEN = 48
            SIZL = SIZEL*1.5
            CALL GSCHH (SIZL)
            STR = 'Press Return (Enter) when finished viewing plot.'
            CALL CVARAR (LEN,STR,LEN,ASTR)
            XSIZ = ANGLEN (LEN,ASTR)
            XPOS = 0.5*(XLEN - XSIZ)
            IF (XPOS .LT. 0) THEN
C             shrink letter size to fit
              SIZL = SIZL*XLEN/XSIZ
              CALL GSCHH (SIZL)
              XPOS = 0.0
            END IF
            YPOS = YLEN + SIZEL    
            HV = 1
            CALL ANGTXA (XPOS,YPOS,LEN,ASTR,HV)
            CALL GSCHH (SIZEL)
C           wait for user to hit any key to continue
            !READ (*,FMT='(A1)') C1
            CALL IGrPause('AP')
          END IF
        END IF
C
        IF (STATE .EQ. 1) THEN
C         deactivate workstation
          CALL GDAWK(WSID)
          WRITE(FE,*) 'DEACTIVATE WORKSTATION', WSID
        END IF
C
        IF (ICLOS .EQ. 1 .OR. IC .EQ. 1) THEN
C         close workstation
          CALL GCLWK(WSID)
          WRITE(FE,*) 'CLOSE WORKSTATION',WSID
C
          IF (IC .EQ. 1) THEN
C           reset graphics mode
            CALL PCGRST (TPAGE,TMODE)
          END IF
C
          IF (IC .EQ. 2) THEN
C           PRIME implementation, so close and reopen GKS
C           close out GKS
            WRITE(FE,*) 'CLOSE GKS for PRIME ERROR'
            CALL GCLKS
C
            IF (IMET .EQ. 1) THEN
C             change file name if Disspla meta file on Prime
              CALL PRMETA (RETCOD)
            END IF
C
C           open GKS and error file and put FE in common block
            CALL GBOPEN (FE)
            WRITE(FE,*) 'REOPEN GKS for PRIME'
          END IF
        END IF
      ELSE
        WRITE(FE,*) 'DNPLTW: Workstation',WSID,' is not open'
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSETUP
     I                   (IC,IM,PREC,FONT,CHXP,CHSP,BCR,BCG,BCB,RR,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     This routine does the level one and two calls for GKS
C     to set the scales, establish the plotting space, set the
C     origin, and allow the axes to be labeled by special routines.
C     This routine does not need to be called when using AIDE
C     routines and DOPLOT.
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETCOD, IC, IM
      INTEGER     PREC, FONT
      REAL      CHSP, CHXP, BCR, BCG, BCB, RR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IC     - computer type (1=pc, 2=prime, 3=dec, 4=sun, 5=aviion)
C     IM     - meta file flag (0=no, 1=yes)
C     PREC   - precision code for text
C     FONT   - font for text
C     CHXP   - character expansion factor (-1.0 for default)
C     CHSP   - character spacing (-1.0 for default)
C     BCR    - fraction of red for background
C     BCG    - fraction of green for background
C     BCB    - fraction of blue for background
C     RR     - symbol size ratio
C     RETCOD - return code    0 = everything ok,
C                             1 = device not setup to plot
C              0 - don't wait
C              1 - wait for user to use any key to continue
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   WSID
C
C     + + + EXTERNALS + + +
      EXTERNAL   GSTUPW
C
C     + + + END SPECIFICATIONS + + +
C
      WSID = 1
C
      CALL GSTUPW (IC,IM,PREC,FONT,CHXP,CHSP,BCR,BCG,BCB,RR,
     I                    WSID,
     O                    RETCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSTUPW
     I                   (IC,IMET,PREC,FONT,CHXP,CHSP,BCR,BCG,BCB,RR,
     I                    WSID,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     This routine does the level one and two calls for GKS
C     to set the scales, establish the plotting space, set the
C     origin, and allow the axes to be labeled by special routines.
C     This routine does not need to be called when using AIDE
C     routines and DOPLOT.  This routine is the same as GSETUP
C     except the workstation id number set by the programmer is used.
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETCOD, IC, IMET, WSID
      INTEGER     PREC, FONT
      REAL      CHSP, CHXP, BCR, BCG, BCB, RR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IC     - computer type (1=pc, 2=prime, 3=dec, 4=sun, 5=aviion)
C     IMET   - meta file flag (0=no, 1=yes)
C     PREC   - precision code for text
C     FONT   - font for text
C     CHXP   - character expansion factor (-1.0 for default)
C     CHSP   - character spacing (-1.0 for default)
C     BCR    - fraction of red for background
C     BCG    - fraction of green for background
C     BCB    - fraction of blue for background
C     RR     - symbol size ratio
C     WSID   - workstation id number set by programmer
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
      INTEGER   ERR,VID,ALH,ALV,ISTATE,DCUNIT,L1,
     &          IERR,COLA,NCOLI,NPCI,I1(13),ISS
      INTEGER   L0, LX, LY, CONID, ACONID(5)
      INTEGER   IA(10),LSTR(5),ERRIND,LDR,LODR
      REAL      RA(1)
      CHARACTER*80 DATREC(10), ODR(10), STR
      REAL      ZERO,X1,X2,Y1,Y2,RXWIN,RYWIN,RXNDC, RYNDC,WINX,WINY,
     &          CHH,CHBX,CHBY,RXMIN,RYMIN,XRPGE,YRPGE,RXDC,RYDC,
     &          RTMP,RTMZ
C
C     + + + INTRINSICS + + +
      INTRINSIC  INT, CHAR, MOD, FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   GSWN, GSVP, GSELNT, GQTXFP, GSCHXP, GQCHXP
      EXTERNAL   GSWKWN, GSWKVP, GQCHH, GQCHSP, GQCHB, GSASF
      EXTERNAL   GSCHSP, GSTXAL, GOPWK, GACWK, GQWKS, GQDSP
      EXTERNAL   GQCF, GSTXFP, GQOPS, GSMKSC, GSCR, GCLRWK
      EXTERNAL   GPREC, GESC
C
C     + + + DATA INITIALIZATIONS + + +
C                                  pc   prime  vax  unix  aviion
      DATA  I1/13*1/, L0/0/, ACONID/0,   10,   10,   0,   -32768/
C
C     + + + END SPECIFICATIONS + + +
C
      ZERO  = 0.0
      L1 = 1
      VID   = 1
      IERR = 0
      RETCOD = 0
      CONID = ACONID(IC)
C
C
      WRITE (FE,*)
      WRITE (FE,*) '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
C     find state of system
      CALL GQOPS (ISS)
      WRITE(FE,*) 'State of system is',ISS
      WRITE(FE,*) 'WORKSTATION ID IS ',WSID
C
C     check on state of workstation
      CALL GQWKS (WSID, IERR, ISTATE)
      IF (IERR .NE. 0) THEN
C       workstation must not be open
C
C       make all text attributes INDIVIDUAL
        CALL GSASF (I1)
C       initialize the GKS environment
        IF (IC .EQ. 5) THEN
C         computer is Aviion with Prior GKS
          IF (DEVCOD .EQ. 1100 .OR. DEVCOD .EQ. 1101) THEN
C           X window size and position
            IA(1) = 1
            IA(2) = 0
            IA(3) = INT(XWINLC(1)*1271.0)
            IA(4) = INT(XWINLC(2)*996.0)
            IA(5) = INT(XWINLC(3)*1271.0)
            IA(6) = INT(XWINLC(4)*996.0)
            STR   = 'GKS WINDOW '//CHAR(WSID+48)
            LSTR(1)= 13
            CALL GPREC(6,IA,0,RA,1,LSTR,STR,12,ERRIND,LDR,DATREC)
            CALL GESC(-9,LDR,DATREC,10,LODR,ODR)
          ELSE
C           not X window, must need a file, so set unit number
            CONID = MTPLUT
          END IF
        END IF
C       open specific workstation
        VID = 1
        WRITE (FE,*) 'ABOUT TO OPEN DEVICE',DEVCOD,' WSID=',WSID
        CALL GOPWK (WSID,CONID,DEVCOD)
      END IF
C
C     Check for meta file for Prime
      IF (IMET .EQ. 0) THEN
C       not a meta file, ok to activate
        WRITE (FE,*) 'ACTIVATE WORKSTATION',WSID
        CALL GACWK (WSID)
        WRITE (FE,*) 'GET WORKSTATION CHARACTERISTICS'
C       clear the workstation
        L1 = 1
        CALL GCLRWK (WSID, L1)
        CALL GQWKS (WSID,IERR,ISTATE)
        WRITE (FE,*) '  WORKSTATION STATE & ERROR CODE',ISTATE,IERR
      ELSE
        ISTATE= 1
      END IF
C
      IF (ISTATE.EQ.0 .OR. IERR .NE. 0) THEN
C       device & not available, can't make plot
        WRITE(FE,*) '  DEVICE',DEVCOD,' NOT AVAILABLE'
        RETCOD = 1
      ELSE
C       inquire about workstations whole space
        CALL GQDSP (DEVCOD,ERR,DCUNIT,RXDC,RYDC,LX,LY)
        WRITE (FE,*) '  JUST INQUIRED ABOUT WORKSTATION, ERROR',ERR
        WRITE (FE,*) '       UNITS,SIZES ARE',DCUNIT,RXDC,RYDC,LX,LY
        IF (DEVCOD .EQ. 1100 .OR. DEVCOD .EQ. 1101) THEN
C         X window size within whole space
          WINX  = (XWINLC(3)-XWINLC(1))
          RXWIN = RXDC * WINX
          WINY  = (XWINLC(4)-XWINLC(2))
          RYWIN = RYDC * WINY
          WRITE (FE,*) '      WINDOW ADJUSTMENT:',WINX,RXWIN,WINY,RYWIN
        ELSE
C         whole space
          RXWIN = RXDC
          RYWIN = RYDC
        END IF
        IF (DEVTYP .EQ. 5 .AND. DEVCOD .EQ. 102) THEN
C         for DISSPLA meta file
          RXDC = 1.0
          RYDC = 1.0
        END IF
        IF (RXDC.GT.1.0E-6 .AND. RYDC.GT.1.0E-6) THEN
C         determine normalized device coordinates
          IF (RXWIN .GT. RYWIN) THEN
            RXNDC = 1.0
            RYNDC = RYWIN/RXWIN
          ELSE
            RYNDC = 1.0
            RXNDC = RXWIN/RYWIN
          END IF
          WRITE(FE,*) '  NORMALIZED DEVICE COORDINATES:',RXNDC,RYNDC
        ELSE
C         dont know size of device, give warning
          WRITE(FE,*) 'Warning: could not get size of device'
        END IF
C
        CALL GQCF (DEVCOD,ERR,NCOLI,COLA,NPCI)
        WRITE (FE,*) '  JUST INQUIRED COLOR',ERR,NCOLI,COLA,NPCI
C
C       set global window (wc)
C       find limiting dimension
        RXMIN = RXWIN/XPAGE
        RYMIN = RYWIN/YPAGE
        WRITE(FE,*) ' X:',XPHYS,XPAGE,RXNDC,RXDC,RXWIN,RXMIN
        WRITE(FE,*) ' Y:',YPHYS,YPAGE,RYNDC,RYDC,RYWIN,RYMIN
        IF (RXMIN .LT. RYMIN) THEN
C         plot wider than screen
          XRPGE= XPAGE
          RTMP = RYWIN/RXWIN
          YRPGE= XPAGE*RTMP
          WRITE (FE,*) ' WIDER:',XRPGE,YRPGE,RTMP
        ELSE
C         plot taller than screen
          YRPGE= YPAGE
          RTMP = RXWIN/RYWIN
          XRPGE= YPAGE*RTMP
          WRITE (FE,*) ' HIGHER:',XRPGE,YRPGE,RTMP
        END IF
        X2 = XRPGE - XPHYS
        Y2 = YRPGE - YPHYS
        X1 = -XPHYS
        Y1 = -YPHYS
        CALL GSWN (VID,X1,X2,Y1,Y2)
        WRITE (FE,*) '  JUST SET GLOBAL WINDOW (WC)',X1,X2,Y1,Y2
C       set global viewport (ndc)
        CALL GSVP (VID,ZERO,RXNDC,ZERO,RYNDC)
        WRITE (FE,*) '  JUST SET GLOBAL VIEWPORT, (NDC)',RXNDC,RYNDC
C       select normalization transformation
        CALL GSELNT (VID)
        WRITE (FE,*)'  JUST SELECTED NORMALIZATION TRANSFORMATION',VID
C
        IF (RXNDC.LT.1.0) THEN
C         check x axis
          RTMP= XPAGE/YPAGE
          IF (RTMP.LT.RXNDC) THEN
            RTMZ = RXDC
            RXDC = RXDC* RTMP/RXNDC
            IF (RXDC.GT.RTMZ) RXDC= RTMZ
            RXNDC= RTMP
          END IF
C         check y axis
          RTMP= YPAGE/XPAGE
          IF (RTMP.GT.RXNDC .OR. RTMP.LT.RYNDC) THEN
            RTMZ = RYDC
            RYDC = RXDC* RTMP
            IF (RYDC.GT.RTMZ) RYDC= RTMZ
            RYNDC= RXNDC* RTMP
          END IF
        ELSE IF (RYNDC.LT.1.0) THEN
C         check y axis
          RTMP= YPAGE/XPAGE
          IF (RTMP.LT.RYNDC) THEN
            RTMZ = RYDC
            RYDC = RYDC* RTMP/RYNDC
            IF (RYDC.GT.RTMZ) RYDC= RTMZ
            RYNDC= RTMP
          END IF
C         check x axis
          RTMP= XPAGE/YPAGE
          IF (RTMP.GT.RYNDC .OR. RTMP.LT.RXNDC) THEN
            RTMZ = RXDC
            RXDC = RYDC* RTMP
            IF (RXDC.GT.RTMZ) RXDC= RTMZ
            RXNDC= RYNDC* RTMP
          END IF
        END IF
C
C       set workstation window (ndc)
        CALL GSWKWN (WSID,ZERO,RXNDC,ZERO,RYNDC)
        WRITE (FE,*) '  JUST SET WORKSTATION WINDOW (NDC)', RXNDC,RYNDC
C       set workstation viewport
        CALL GSWKVP (WSID,ZERO,RXDC,ZERO,RYDC)
        WRITE (FE,*) '  JUST SET WORKSTATION VIEWPORT (DC)',RXDC,RYDC
C
C       set text font and precision
        CALL GSTXFP (FONT, PREC)
        CALL GQTXFP (ERR,FONT,PREC)
        WRITE (FE,*) '  FONT, PRECISION, ERROR CODE ARE ',FONT,PREC,ERR
C
C       set expansion factor
        CALL GSCHXP (CHXP)
        CALL GQCHXP (ERR,CHXP)
        WRITE (FE,*) '  EXPANSION FACTOR AND ERROR CODE',CHXP,ERR
C
        CALL GSCHSP (CHSP)
        CALL GQCHSP (ERR,CHSP)
        WRITE (FE,*) '  CHARACTER SPACING AND ERROR CODE',CHSP,ERR
C
        ALH = 0
        ALV = 0
C       write(*,*) '  vertical text alignment (1-6)'
C       read(*,*) alv
C       write(*,*) '  horizontal text alignment(1-4)'
C       read(*,*) alh
        CALL GSTXAL (ALH, ALV)
        WRITE (FE,*) '  TEXT ALIGNMENT ',ALH,ALV
C
        CALL GQCHH (ERR,CHH)
        WRITE (FE,*) '  CHARACTER HEIGHT (WC) AND ERROR CODE',CHH,ERR
        CALL GQCHB (ERR,CHBX,CHBY)
        WRITE (FE,*) '  BASE VECTOR AND ERROR CODE',CHBX,CHBY,ERR
C
C       set background color
        CALL GSCR (WSID,L0,BCR,BCG,BCB)
C
C       Set symbol size ratio
        CALL GSMKSC(RR)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GRVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     graph library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END

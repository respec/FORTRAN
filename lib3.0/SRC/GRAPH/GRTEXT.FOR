C
C
C
      SUBROUTINE   LEGNDC
     I                    (K,
     M                     LEGNDS)
C
C     + + + PURPOSE + + +
C     This routine writes the legend for a curve.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEGNDS, K
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - order number of curve in data array
C     LEGNDS - counter for number of curves in legend
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J, IHV, ERR
      INTEGER   I2,I3,ITMP,I1,I5
      REAL      LABX(2), LABY(2), LABX4(5), LABY4(5), YP, XP, YYLEN,
     $          FAC, HGT
      CHARACTER*20  LABEL
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL,ABS
C
C     + + + FUNCTIONS + + +
      INTEGER   CKNBLK
C
C     + + + EXTERNALS + + +
      EXTERNAL   GSFAIS, GSFASX, GFA, GPL, GMARK1, ANGTX, GSCHUP
      EXTERNAL   GSLN, CKNBLK, GQMKSC, GSTXCI, GSFASI
C
C     + + + OUTPUT FORMATS + + +
 2020 FORMAT(80A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     get scaling factor and set symbol height
      CALL GQMKSC (ERR,FAC)
      IF (ERR .NE. 0 .OR. ABS(FAC).LT.0.01) FAC = 1.0
      HGT = SYMSIZ(K)*FAC
C
      I1= 1
      I2= 2
      I3= 3
      I5= 5
      I = 20
      J = CKNBLK(I,LBC(1,K))
      IF (J .GT. 0) THEN
        CALL GSCHUP (0.0,1.0)
        YYLEN= YLEN
        IF (ALEN .GT. 0.0001) YYLEN= YYLEN- ALEN- 1.5* SIZEL
C
C       add line to legend.
        IF (LOCLGD(1).LT.-0.5) THEN
C         default location
          LABX(1)= 5.0* SIZEL
          LABX(2)= 10.0* SIZEL
          LABY(1)= YYLEN- (2.0* REAL(LEGNDS)+ 1.0)* SIZEL
        ELSE
C         user's location
          LABX(1)= XLEN*(LOCLGD(1))
          LABX(2)= LABX(1) + 5.0*SIZEL
          LABY(1)= LOCLGD(2)* YYLEN- (2.* (REAL(LEGNDS- 1))+ 1.)* SIZEL
        END IF
        LABY(2)= LABY(1)
C
        IF (PATTRN(K).NE.0) THEN
C         use shading for legend
          LABY(1)= LABY(1)- 0.5*SIZEL
          LABY(2)= LABY(2)- 0.5*SIZEL
          DO 10 I= 1,4
            LABX4(I)= LABX(I/3+ 1)
            LABY4(I)= LABY(I/3+ 1)
 10       CONTINUE
          LABY4(2)= LABY4(2)+ SIZEL
          LABY4(3)= LABY4(3)+ SIZEL
          LABX4(5)= LABX4(1)
          LABY4(5)= LABY4(1)
C
          IF (PATTRN(K).NE.1) THEN
C           not hollow
            IF (PATTRN(K) .EQ. 2) THEN
C             solid
              CALL GSFAIS (I1)
            ELSE
C             hatch of some kind
              CALL GSFAIS (I3)
              CALL GSFASX (PATTRN(K))
              CALL GSFASI (PATTRN(K))
            END IF
C           fill it
            CALL GFA (I5,LABX4,LABY4)
          END IF
C
C         do border
          CALL GPL (I5,LABX4,LABY4)
          LABY(1)= LABY(1)+ 0.5* SIZEL
          LABY(2)= LABY(2)+ 0.5* SIZEL
        END IF
C
        IF (SYMBL(K).GT.0) THEN
C         using symbol, show it
          CALL GMARK1(SYMBL(K),HGT,LABX(1),LABY(1))
          CALL GMARK1(SYMBL(K),HGT,LABX(2),LABY(2))
        END IF
        IF (LNTYP(K).NE.0 .AND. PATTRN(K).EQ.0) THEN
C         using line, show it
          ITMP= LNTYP(K)
          CALL GSLN (ITMP)
          CALL GPL (I2,LABX,LABY)
        END IF
C
        YP= LABY(1)- 0.5* SIZEL
        XP= LABX(2)+ 3.0* SIZEL
        WRITE(LABEL,2020) (LBC(J,K),J=1,20)
C                    NOTE: Set attributes for text?
C       set color to black/white
        CALL GSTXCI (LBCOLR)
        IHV = 1
        CALL ANGTX (XP,YP,LABEL,IHV)
        LEGNDS= LEGNDS+ 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADDTXT
     I                    (XP,YP,SIZEL,NCHR,CPR,CTXT)
C
C     + + + PURPOSE + + +
C     This routine adds text to the plot.
C
C     + + + KEYWORDS + + +
C     DISSPLA
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XP,YP,SIZEL
      CHARACTER*1   CTXT(120)
      INTEGER   NCHR,CPR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XP     - horizontal location on plot, in world coordinates
C     YP     - vertical location on plot, in world coordinates
C     SIZEL  - height of lettering, in world coordinates
C     NCHR   - number of characters to use (max of 120)
C     CPR    - characters per line
C     CTXT   - text to be written
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*120   CHR
      INTEGER   I,I1,I2, IHV
      REAL   XPOS, YPOS
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANGTX, GSCHUP
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(120A1)
C
C     + + + END SPECIFICATIONS + + +
C
C
      I1 = 1
      I2 = CPR
      XPOS = XP
      YPOS = YP
      CALL GSCHUP (0.0,1.0)
C
 10   CONTINUE
        WRITE(CHR,2000) (CTXT(I),I=I1,I2)
        IHV = 1
        CALL ANGTX (XPOS,YPOS, CHR,IHV)
        I1 = I1 + CPR
        I2 = I2 + CPR
        IF (I2 .GT. NCHR) I2 = NCHR
        YPOS = YPOS - 1.5*SIZEL
      IF (I1 .LT. NCHR) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   MATCHL
     I                  (AUXMAX,SIZEL,PMIN,PMAX,YTYPE,TICS,
     O                   RMAX1)
C
C     + + + PURPOSE + + +
C     This routine finds position to line up axes labels when both
C     main and auxilary plots are used.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YTYPE, TICS
      REAL   AUXMAX,SIZEL,PMIN,PMAX,RMAX1
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AUXMAX - maximum scale value for auxilary axis
C     SIZEL  - height of lettering, in world coordinates
C     PMIN   - minimum value to be plotted on main axis
C     PMAX   - maximum value to be plotted on main axis
C     YTYPE  - scale for Y-axis (left first, right second)
C              0 - none
C              1 - arithmetic
C              2 - logarithmic
C     TICS   - number of tic marks on left y-axis
C     RMAX1  - offset in world coordinates for bottom of axis label
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SIGDIG, DECPLA, LEN, OLEN, IFLG, MXLEN, IDUM,
     &          MAXY, MINY, I
      REAL      XDUM, YINC, XPMIN, XP, YVALUE, RMAX2
      CHARACTER*1 STR(12)
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, INT, ALOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECCHX, CLNSFT, ADCOMA, LENSTR
C
C     + + + END SPECIFICATIONS + + +
C
C
C       Determine max # char to label axis
        XDUM = AUXMAX/2.0
        SIGDIG = 5
        DECPLA = 4
        LEN = 12
        IFLG = 1
        CALL DECCHX (XDUM, LEN, SIGDIG, DECPLA, STR)
        CALL CLNSFT (LEN, IFLG, STR, OLEN,IDUM)
        IF (XDUM .GE. 1000.0) THEN
          CALL ADCOMA (LEN,STR)
          OLEN = LENSTR (LEN,STR)
        END IF
        MXLEN = OLEN
        CALL DECCHX (AUXMAX, LEN, SIGDIG, DECPLA, STR)
        CALL CLNSFT (LEN, IFLG, STR, OLEN,IDUM)
        IF (AUXMAX .GE. 1000.0) THEN
          CALL ADCOMA (LEN,STR)
          OLEN = LENSTR (LEN,STR)
        END IF
        IF (OLEN .GT. MXLEN) MXLEN = OLEN
        RMAX1 = -0.8*SIZEL*(REAL(MXLEN)+1.0)
        RMAX1 = RMAX1 - SIZEL
C
C       now find position for left Y-axis
        IF (YTYPE .EQ. 1) THEN
C         arithmetic axis
          YVALUE = PMIN
          YINC = (PMAX-PMIN)/REAL(TICS)
          IFLG = 1
          CALL DECCHX (YINC, LEN, SIGDIG, DECPLA, STR)
          CALL CLNSFT (LEN, IFLG, STR, OLEN, DECPLA)
          IFLG = 0
          XPMIN = -SIZEL
          DO 230 I=1,11
            CALL DECCHX (YVALUE, LEN, SIGDIG, DECPLA, STR)
            CALL CLNSFT (LEN, IFLG, STR, OLEN,IDUM)
            IF (YVALUE .GE. 1000.0) THEN
              CALL ADCOMA (LEN,STR)
              OLEN = LENSTR (LEN,STR)
            END IF
            XP = -0.8*SIZEL*(REAL(OLEN) + 1.5)
            IF (XP.LT.XPMIN) XPMIN = XP
            YVALUE = YVALUE + YINC
 230      CONTINUE
          RMAX2 = XPMIN - SIZEL
        ELSE IF (YTYPE .EQ. 2) THEN
C         log axis
          XPMIN = -SIZEL
          MAXY = INT(1.01*ALOG10(PMAX))
          MINY = INT(1.01*ALOG10(PMIN))
          IFLG = 1
          IF (MINY.GE.3 .AND. MAXY.LE.5) THEN
            DO 240 I=MINY,MAXY
              YVALUE = 10.0**I
              CALL DECCHX (YVALUE, LEN, SIGDIG, DECPLA, STR)
              CALL CLNSFT (LEN, IFLG, STR, OLEN,IDUM)
              IF (YVALUE .GE. 1000.0) THEN
                CALL ADCOMA (LEN,STR)
                OLEN = LENSTR (LEN,STR)
              END IF
              XP = -0.8*SIZEL*(REAL(OLEN) + 1.0)
              IF (XP.LT.XPMIN) XPMIN = XP
 240        CONTINUE
          ELSE
C           big numbers so using logs
            XPMIN = -0.8*SIZEL*6.0
          END IF
          RMAX2 = XPMIN - SIZEL
        END IF
        IF (RMAX2 .LT. RMAX1) RMAX1 = RMAX2
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLTITL
     I                    (TITLE,SIZEL,XLEN,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine plots the title at the bottom of the plot.
C     Can plot up to 3 lines.  Next lines are indicated in the TITLE
C     string with an '&'.  The first line is centered, ignoring
C     leading and trailing blanks.  The second and third lines start
C     in the same position as the first line so are not centered.
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENT + + +
      CHARACTER*1 TITLE(240)
      REAL        SIZEL,XLEN,BOTOM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TITLE  - title of plot
C     SIZEL  - height of lettering, in world coordinates
C     XLEN   - length of X-axis, in world coordinates
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LOC1,LOC2,LOC3,J,L1,L240,TLEN,MXL,TOTLEN,IHV
      CHARACTER*1 NXTLNE(1), BLNK(1), TITL(240)
      REAL        YP,XP
C
C     + + + FUNCTIONS + + +
      INTEGER   STRFND, LENSTR
      REAL   ANGLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   LFTSTR, STRFND, LENSTR, CHRCHR
      EXTERNAL   ANGTXA, GSCHUP, ANGLEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   L1,     L240,      NXTLNE
     &      / 1,      240,        '&'/
C
C     + + + END SPECIFICATIONS + + +
C
C     put title in local array
      CALL CHRCHR (L240, TITLE, TITL)
C
      CALL GSCHUP (0.0,1.0)
C     maximum space available in characters
      MXL = INT(1.2*XLEN/(0.85*SIZEL))
C     limit maximum size of single line to 149 characters     
      IF (MXL .GT. 149) MXL = 149
      CALL LFTSTR (L240,TITL)
      TOTLEN = LENSTR (L240, TITL)
C     TOTLEN is total length less trailing blanks
C
      IF (TOTLEN .GT. 0) THEN
C       locate position before first '&'
        LOC1 = STRFND(TOTLEN,TITL,L1,NXTLNE) - 1
        IF (LOC1 .LE. 0) LOC1 = TOTLEN
C       get length without trailing blanks to first '&'
        TLEN = LENSTR(LOC1,TITL(1))
C       limit length to space available
        IF (TLEN .GT. MXL) TLEN = MXL
C       plot the first line
        YP = BOTOM - 2.0*SIZEL
        XP = 0.5*XLEN - 0.5*ANGLEN(TLEN,TITL(1))
        IHV = 1
        IF (TLEN .GT. 0) CALL ANGTXA (XP,YP,TLEN,TITL,IHV)
C
        IF (TLEN .LT. TOTLEN) THEN
C         check for non-blanks for a second line
          LOC1 = LOC1 + 2
C         get length of remaining string to process
          J = L240 + 1 - LOC1
C         find next '&'
          LOC2 = STRFND(J,TITL(LOC1),L1,NXTLNE)
          IF (LOC2 .GT. 0) THEN
            LOC2 = LOC2 + LOC1 - 2
          ELSE
            LOC2 = TOTLEN
          END IF
          J = LOC2 + 1 - LOC1
          IF (J.GT.0) THEN
            TLEN= LENSTR(J,TITL(LOC1))
          ELSE
            TLEN= 0
          END IF
          IF (TLEN .GT. 0) THEN
C           plot the second line
            YP = YP - 1.75*SIZEL
            IHV = 1
            CALL ANGTXA (XP,YP,TLEN,TITL(LOC1),IHV)
            IF (LOC2 .LT. 238) THEN
C             check for non-blanks for a third line (same as above)
              LOC2 = LOC2 + 2
              J = L240 + 1 - LOC2
              LOC3 = STRFND(J,TITL(LOC2),L1,NXTLNE)
              IF (LOC3 .GT. 0) THEN
                LOC3 = LOC3 + LOC2 - 2
              ELSE
                LOC3 = TOTLEN
              END IF
              J= LOC3- LOC2+ 1
              IF (J.GT.0) THEN
                TLEN= LENSTR(J,TITL(LOC2))
              ELSE
                TLEN= 0
              END IF
              IF (TLEN .GT. 0) THEN
C               plot the third line
                YP = YP - 1.75*SIZEL
                IHV = 1
                CALL ANGTXA (XP,YP,TLEN,TITL(LOC2),IHV)
              END IF
            END IF
          END IF
        END IF
      END IF
C
C     try to flush out buffer
      XP = XP - SIZEL
      TLEN = 1
      BLNK(1) = ' '
      CALL ANGTXA(XP,YP,TLEN,BLNK,IHV)
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANGTX
     I                   (XPOS,YPOS,STR,HV)
C
C     + + + PURPOSE + + +
C     routine to write out graphics strings of any size one
C     character at a time.  It converts a character*(*) string
C     into a character*1 array, then call routine to write it out
C
C     + + + DUMMY ARGUMENTS + + +
      REAL          XPOS,YPOS
      CHARACTER*(*) STR
      INTEGER   HV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XPOS   - x-coordinate of position to start writing string
C     YPOS   - y-coordinate of position to start writing string
C     STR    - character string
C     HV     - 1-horizontal lettering,   2- vertical lettering
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,CLEN
      CHARACTER*1 CSTR(240)
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL    ANGTXA
C
C     + + + DATA INITIALIZATIONS + + +
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (240A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     get length of character string
      CLEN= LEN(STR)
C     read string into character*1 array
      READ (STR,1000) (CSTR(I),I=1,CLEN)
C
C     call routine to output character*1 array
      CALL ANGTXA (XPOS,YPOS,CLEN,CSTR,HV)
C
      RETURN
      END
C
C
C
      SUBROUTINE   AUXAXE
     I                    (XLEN,YLEN,ALEN,SIZEL,YALABL,TICS,PMIN,PMAX,
     I                     RMAX)
C
C     + + + PURPOSE + + +
C     This routine labels the axis for the auxilary plot.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICS
      REAL        XLEN,YLEN,SIZEL,RMAX,ALEN,PMIN,PMAX
      CHARACTER*1 YALABL(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of x-axis, in world coordinates
C     YLEN   - length of y-axis or length of main Y-axis and
C              auxilary axis plus small space between, in world
C              coordinates
C     ALEN   - auxilary plot axis length
C     SIZEL  - height of lettering, in world coordinates
C     YALABL - label for auxilary plot on top
C     TICS   - number of tics on axis less one on bottom
C     PMIN   - minimum value for axis
C     PMAX   - maximum value for axis
C     RMAX   - inches from axis for bottom of label
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,NL,OLEN,LEN,DECPLA,SIGDIG,K,IDUM, IHV, ITMP,
     #            IFLG,MAX,POS1(6),POS2(6),TLEN,IPOS,IB
      REAL        XP,YP,ONE,BASE,ZERO,MI,DIF,VAL,XPP(5),YPP(5)
      CHARACTER*1 STR(14), NXTLNE
C
C     + + + FUNCTIONS + + +
      INTEGER   STRLNX
      REAL     ANGLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECCHX, CLNSFT, STRLNX, LFTSTR, GPL, ANGLEN
      EXTERNAL   GSPLCI, GSPMCI, GSFACI, GSTXCI, ANGTXA, GSCHUP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NXTLNE/'&'/, MI/-1.0/, ZERO/0.0/, ONE/1.0/
C
C     + + + END SPECIFICATIONS + + +
C
C     set color code for axes
      IB = 1
C
      LEN   = 14
      DECPLA= 4
      SIGDIG= 5
      BASE= YLEN+ 1.5* SIZEL
C
C     add label
      CALL GSPLCI (IB)
      CALL GSPMCI (IB)
      CALL GSFACI (IB)
      CALL GSTXCI (IB)
C
C     Make box, begin and end at lower left
      YPP(1) = BASE
      YPP(2) = BASE + ALEN
      YPP(3) = BASE + ALEN
      YPP(4) = BASE
      YPP(5) = BASE
      XPP(1) = 0.0
      XPP(2) = 0.0
      XPP(3) = XLEN
      XPP(4) = XLEN
      XPP(5) = 0.0
      ITMP = 5
      CALL GPL (ITMP, XPP, YPP)
C     add tics
      DIF = ALEN/REAL(TICS)
      ITMP = 2
      XPP(1) = 0.0
      XPP(2) = 0.5*SIZEL
      DO 4 I = 2,TICS
        YPP(1) = BASE + REAL(I-1)*DIF
        YPP(2) = YPP(1)
        CALL GPL (ITMP, XPP, YPP)
 4    CONTINUE
      XPP(1) = XLEN
      XPP(2) = XLEN - 0.5*SIZEL
      DO 6 I = 2,TICS
        YPP(1) = BASE + REAL(I-1)*DIF
        YPP(2) = YPP(1)
        CALL GPL (ITMP, XPP, YPP)
 6    CONTINUE
C
      YP  = BASE- 0.4* SIZEL
      XP  = -0.8* SIZEL* 2.0
      OLEN= 1
      IHV = 1
      IFLG = 1
      DIF = (PMAX-PMIN)/REAL(TICS)
      DO 8 K = 1,TICS+1
        VAL = PMIN + DIF*REAL(K-1)
        CALL DECCHX (VAL,LEN,SIGDIG,DECPLA,STR)
        CALL CLNSFT (LEN,IFLG,STR,OLEN,IDUM)
        XP= -0.8* SIZEL* (REAL(OLEN)+ 1.0)
        YP= BASE+ REAL(K-1)*ALEN/REAL(TICS)- 0.5* SIZEL
        CALL ANGTXA (XP,YP,OLEN,STR,IHV)
 8    CONTINUE
C
C     determine max # char/line
      MAX= INT(ALEN/(0.75*SIZEL))+ 2
      IF (MAX.GT.40) MAX= 40
C     determine number of lines
      NL = 1
      DO 10 K= 1,80
        IF (YALABL(K).EQ.NXTLNE .AND. NL.LT.7) THEN
          POS2(NL)= K
          NL= NL+ 1
        END IF
 10   CONTINUE
C
C     set begin and end positions of each line in buffer
      POS1(1)= 1
      K= 1
 12   CONTINUE
        POS1(K+1)= POS2(K)+ 1
        POS2(K)  = POS2(K)- 1
        K= K+ 1
      IF (K.LT.NL) GO TO 12
      POS2(NL)= 80
C
C     write(fe,*) 'max,nl=',max,nl
C     write(fe,*) 'pos1=',pos1
C     write(fe,*) 'pos2=',pos2
C     write(fe,*) 'rmax=',rmax
C     set to write up side
      CALL GSCHUP (MI,ZERO)
C     plot the lines
      DO 20 I= 1,NL
        LEN = POS2(I)+ 1- POS1(I)
        IPOS = POS1(I)
        CALL LFTSTR(LEN,YALABL(IPOS))
        TLEN= STRLNX(LEN,YALABL(IPOS))
C       write(fe,*) 'i,len,tlen=',i,len,tlen
        IF (TLEN.GT.0) THEN
          IF (TLEN.GT.MAX) TLEN= MAX
          XP= RMAX- REAL(NL- I)* 1.75* SIZEL
          YP = BASE + ALEN*0.5 - 0.5*ANGLEN (TLEN,YALABL(IPOS))
          IHV = 2
          CALL ANGTXA (XP,YP,TLEN,YALABL(IPOS),IHV)
        END IF
 20   CONTINUE
C
      CALL GSCHUP (ZERO,ONE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   LABYAX
     I                    (LBAXIS,SIZEL,AUX,XPMIN,LFTRT,XLEN,YLEN)
C
C     + + + PURPOSE + + +
C     This routine titles the left or right y-axis.
C
C     + + + KEYWORDS + + +
C     DISSPLA
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LFTRT
      REAL        SIZEL,AUX,XPMIN,XLEN,YLEN
      CHARACTER*1 LBAXIS(80)
C
C     + + + ARGUMENT DEFINITION + + +
C     LBAXIS - character string for axis label
C     SIZEL  - height of lettering, in world coordinates
C     AUX    - location of label if < 0.0 for alignment with auxilary
C              plot
C     XPMIN  - minimum distance title must be from axis in
C              world coordinates
C     LFTRT  - axis indicator
C              1 - left axis
C              2 - right axis
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between, in world
C              coordinates
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IPOS,ILEN,LEN,L80,L1,IRPT, IHV
      REAL         XP,YP,M1,ZERO
      CHARACTER*1  NXTLNE(1)
C
C     + + + FUNCTIONS + + +
      INTEGER   STRLNX, STRFND
      REAL      ANGLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   STRLNX, LFTSTR, STRFND, ANGTXA, GSCHUP, ANGLEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L1, L80, NXTLNE /1,80,'&'/
C
C     + + + END SPECIFICATIONS + + +
C
      M1  = -1.0
      ZERO= 0.0
      CALL GSCHUP (M1,ZERO)
C     IRPT is a repeat flag for multiple lines of axis title
      IRPT= 1
      IPOS= 1
      ILEN= STRFND(L80,LBAXIS,L1,NXTLNE) - 1
      IF (ILEN.LE.0) THEN
        ILEN= 80
        IRPT= 0
      END IF
C
 40   CONTINUE
C       determine x position of line
        IF (LFTRT.EQ.2) THEN
C         axis on right side
          XP  = XLEN+ 2.0* SIZEL + ABS(XPMIN)
          IF (IRPT.EQ.2) THEN
            XP= XP+ 1.75* SIZEL
          END IF
        ELSE IF (AUX.LT.0.0) THEN
C         auxilary axis used, so must align axis on left
          XP= AUX
          IF (IRPT.EQ.1) THEN
            XP= XP- 1.75* SIZEL
          END IF
        ELSE
C         axis on left side         
          XP= -ABS(XPMIN)- SIZEL
          IF (IRPT.EQ.1) THEN
            XP= XP- 1.75* SIZEL
          END IF
        END IF
C
C       determine y position of line
        CALL LFTSTR(ILEN,LBAXIS(IPOS))
        LEN= STRLNX(ILEN,LBAXIS(IPOS))
        YP = YLEN*0.5 - 0.5*ANGLEN (LEN,LBAXIS(IPOS))
        IHV = 2
        IF (LEN.GT.0) CALL ANGTXA (XP,YP,LEN,LBAXIS(IPOS),IHV)
C
        IF (IRPT.EQ.1) THEN
          IRPT= 2
          IPOS= ILEN+ 2
          IF (IPOS.GT.80) IRPT= 0
          ILEN= L80+ 1- IPOS
        ELSE
          IRPT= 0
        END IF
      IF (IRPT.EQ.2) GO TO 40
C
      RETURN
      END
C
C
C
      SUBROUTINE   LABXAX
     I                    (LBAXIS,SIZEL,XLEN,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine titles the x axis.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL        SIZEL,XLEN,BOTOM
      CHARACTER*1 LBAXIS(80)
C
C     + + + ARGUMENT DEFINITION + + +
C     LBAXIS - character string for axis label
C     SIZEL  - height of lettering, in world coordinates
C     XLEN   - length of X-axis, in world coordinates
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LEN,ILEN,IPOS,L1,L80, IHV
      REAL        XP,YP,ZERO,ONE
      CHARACTER*1 NXTLNE(1)
C
C     + + + FUNCTIONS + + +
      INTEGER   STRLNX, STRFND
      REAL   ANGLEN
C
C     + + + INTRINSICS + + +
C
C     + + + EXTERNALS + + +
      EXTERNAL    LFTSTR, STRFND, STRLNX, ANGTXA, GSCHUP, ANGLEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA L1, L80, NXTLNE /1, 80, '&'/
C
C     + + + END SPECIFICATIONS + + +
C
      ZERO= 0.0
      ONE = 1.0
      CALL GSCHUP (ZERO,ONE)
      ILEN= STRFND(L80,LBAXIS,L1,NXTLNE)- 1
      IPOS= 1
      IF (ILEN.LE.0) ILEN= 80
      CALL LFTSTR (ILEN,LBAXIS)
      LEN= STRLNX(ILEN,LBAXIS(IPOS))
      XP = XLEN*0.5 - 0.5*ANGLEN(LEN,LBAXIS(IPOS))
      YP = -4.0* SIZEL
      IHV = 1
      IF (LEN.GT.0) CALL ANGTXA (XP,YP,LEN,LBAXIS(IPOS),IHV)
C
      IF (ILEN.LT.79) THEN
C       must be second line
        IPOS= ILEN+ 2
        ILEN= L80+ 1- IPOS
        CALL LFTSTR(ILEN,LBAXIS(IPOS))
        LEN = STRLNX(ILEN,LBAXIS(IPOS))
        XP = XLEN*0.5 - 0.5*ANGLEN(LEN,LBAXIS(IPOS))
        YP  = -5.75* SIZEL
        IHV = 1
        IF (LEN.GT.0) CALL ANGTXA (XP,YP,LEN,LBAXIS(IPOS),IHV)
      END IF
C
      BOTOM= YP
C
      RETURN
      END
C
C
C
      REAL FUNCTION   ANGLN
     I                       (SLEN,CBUF)
C
C     + + + PURPOSE + + +
C     This routine computes the length of a character string in world
C     corrdinates using GKS routines.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SLEN
      CHARACTER*(*) CBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SLEN   - length of character string
C     CBUF   - character string to be plotted
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*1 CSTR(240)
      INTEGER   I, CLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN
C
C     + + + FUNCTIONS + + +
      REAL  ANGLEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANGLEN
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (240A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     get length of character string
      CLEN= LEN(CBUF)
      IF (CLEN .GT. SLEN) CLEN = SLEN
      IF (CLEN .GT. 240) CLEN = 240
C
C     read string into character*1 array
      READ (CBUF,1000) (CSTR(I),I=1,CLEN)
C
      ANGLN = ANGLEN(SLEN,CSTR)
C
      RETURN
      END

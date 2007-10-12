C
C
C
      SUBROUTINE   FRQPLG
     I                   (HEADNG,NPKPLT,PKLOG,SYSPP,WRCPP,WEIBA,
     I                    NPLOT,SYSRFC,WRCFC,FCXPG,HSTFLG,
     I                    NOCLIM, CLIML, CLIMU, PLTIND, GFORM )
C
C     + + + PURPOSE + + +
C     Log-normal frequency plot using GKS.  Plots four peak flow curves
C     for the observed peaks and estimate peaks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NPKPLT,NPLOT,HSTFLG, NOCLIM, PLTIND
      REAL          PKLOG(NPKPLT),SYSPP(NPKPLT),WRCPP(NPKPLT),
     &              SYSRFC(NPLOT),WRCFC(NPLOT),FCXPG(NPLOT),WEIBA,
     $              CLIML(NPLOT), CLIMU(NPLOT)
      CHARACTER*3   GFORM
      CHARACTER*80  HEADNG(9)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HEADNG - 9 line header
C     NPKPLT - number of observed peaks
C     PKLOG  - log10 of observed peaks (plot with x and o)
C     SYSPP  - systematic record standard deviates (-9999 for historic
C              peaks) (plot with o) (prob non-exceedance)
C     WRCPP  - WRC estimated standard deviates  (plot with x)
C              (prob non-exceedance)
C     WEIBA  - coefficient for plotting position
C     NPLOT  - number of plot points in fitted curve
C     SYSRFC - log10 ordinates of fitted curve, systematic record
C              (plot with dashed line)
C     WRCFC  - log10 ordinates of fitted curve, WRC estimates
C              (plot with solid line)
C     FCXPG  - tabular abscissa standard deviates for fitted curve
C              (plot with dashed and solid line)
C     HSPFLG - flag for use of historic info, 0-used, 1-not used
C     NOCLIM - flag for confidence limits, 0-available, 1-not available
C     CLIML  - log10 ordinates of fitted curve, lower confidence limits
C     CLIMU  - log10 ordinates of fitted curve, upper confidence limits 
C     PLTIND - index number of this station
C     GFORM  - graphics format for plot,
C              CGM - color graphics metafile
C              BMP - bitmap
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxpk.inc'
C
      INTEGER      MXCV, MXXY
      PARAMETER    ( MXCV=6, MXXY=2*MXCV )
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, J, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11,
     $             L12, L15, L20, L21, L30, L33, L40, L50, L80, L240,
     $             LENOBS, OLEN, JUST,
     $             NPLOT1, NPLOT2, NCRV, NVAR, NCHR, LOC, LST, CPR,
     $             XTYP, YTYP(2), IDX, KNTCV, RETCOD,
     $             WSID, ICLOS, IWAIT, CMPTYP,
     $             CTYPE(MXCV), BVFLG(MXCV), SYMBL(MXCV),
     $             LNTYP(MXCV), COLOR(MXCV), PATTRN(MXCV),
     $             TRANSF(MXXY), WHICH(MXXY),
     $             NPLCL1, NPLCL2
      REAL         PP0, PP1, EPSILN, FYT, FXT, TICS, SIZEL, LOCLGD(2),
     $             XPAGE, YPAGE, XPHYS, YPHYS, XLEN, YLEN, ALEN,
     $             PMIN, PMAX, YX(MXPK), PLMN(MXCV), PLMX(MXCV),
     $             VMIN(MXXY), VMAX(MXXY)
      CHARACTER*20 S3,S4,S5,S6,S7,S8
      CHARACTER*40 S2
      CHARACTER*50 S1
      CHARACTER*32 C0,C1,C2,C3
      CHARACTER*33 CW
      CHARACTER*1  TITL(240), CTXT(120), YXLABL(80), YLABL(80),
     &             BLNKLN(80), BLNK, LBV(160), XLABL(80),
     $             LBC(20,MXCV)
      CHARACTER*3  INDSTR
      CHARACTER*12 FNAME
      CHARACTER*16 CDATE
      CHARACTER*80 LSTR
      integer fe
      logical ltf
C
C     + + + FUNCTIONS + + +
      INTEGER  STRLNX
C
C     + + + EXTERNALS + + +
      EXTERNAL  STRLNX, CVARAR, SCALIT, LFTSTR, DECCHR, GPLBXB
      EXTERNAL  GPNCRV, GPLABL, GPTEXT, GPDATA, GPSCLE, GPSIZE, GPCURV
      EXTERNAL  GPLEDG, ZIPC, GPWCXY, GPVAR, PSTUPW, PLTONE, PDNPLT
      EXTERNAL  ANPRGT, IOsRenameFile
C
C     + + + DATA INITIALIZATIONS + + +
      DATA       PP1,     PP0,  EPSILN
     #    /   -2.577,  +2.879,  1.0E-6 /
      DATA L15,L20,L21,L30,L33,L40,L50,L80,L240
     #    / 15, 20, 21, 30, 33, 40, 50, 80, 240 /
      DATA L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12
     #    / 1,  2,  3,  4,  5,  6,  7,  8,  9,  10,  11,  12 /
      DATA TICS, ALEN, BLNK, BLNKLN, YXLABL, LBV
     #   /    0,    0,  ' ', 80*' ', 80*' ', 160*' ' /
      DATA  PATTRN /   0,   0,   0,   0,   0,   0 /
      DATA  COLOR  /   1,   1,   1,   1,   1,   1 /
      DATA  BVFLG  /   4,   4,   4,   4,   4,   4 /
      DATA  WHICH  / 1,4, 1,4, 1,4, 1,4, 1,4, 1,4 /
      DATA  TRANSF / 2,0, 2,0, 2,0, 2,0, 2,0, 2,0 /
      DATA S1 / 'ANNUAL PEAK DISCHARGE & CUBIC FEET PER SECOND     ' /
      DATA S2 / 'ANNUAL EXCEEDANCE PROBABILITY, PERCENT  ' /
      DATA S3 / 'Bull. 17-B frequency' /
      DATA S4 / 'Systematic peaks    ' /
      DATA S5 / 'Historical adjusted ' /
      DATA S6 / 'Systematic frequency' /
      DATA S7 / 'Confidence limits   ' /
      DATA S8 / '                    ' /
      DATA C0 / 'Peakfq 5 run                  ' /
      DATA C1 / 'NOTE - Preliminary computation' /
      DATA C2 / 'User is responsible for       ' /
      DATA C3 / 'assessment and interpretation.' /
      DATA CW /'& Plotting position with WEIBA = ' /
C
C     + + + END SPECIFICATIONS + + +
C
C     plot 8 variables and 4 curves
Ckmf  Nov 2000 add confidence limits
      NCRV = 4 - HSTFLG
Ckmf  Nov 2000 add confidence limits
Ckmf  comment out for now
      IF (NOCLIM .NE. 1) NCRV = NCRV + 2
      NVAR = 2*NCRV
      CALL GPNCRV (NCRV, NVAR)
C
C     Y-axis label
C     'ANNUAL PEAK DISCHARGE, IN CFS'
      CALL ZIPC (L80, BLNK, YLABL)
      CALL CVARAR (L50,S1,L50,YLABL)
C
C     X-axis label
C     'ANNUAL EXCEEDANCE PROBABILITY'
      CALL ZIPC (L80, BLNK, XLABL)
      CALL CVARAR (L40,S2,L40,XLABL)
      CALL GPLBXB (XLABL)
C
C     id, title, years
      CALL ZIPC (L240, BLNK, TITL)
      CALL CVARAR (L80,HEADNG(9),L80,TITL(1))
      CALL LFTSTR(L80,TITL(1))
C
      IF (WEIBA .GT. EPSILN) THEN
C       non-standard coefficient for plotting position
        LOC = STRLNX (L80,TITL) + 1
        CALL CVARAR (L33,CW,L33,TITL(LOC))
        LOC = LOC + L33
        JUST = 1
        CALL DECCHR (WEIBA,L7,JUST,OLEN,TITL(LOC))
      END IF
C
Cprh     add date to title
Cprh      LOC = STRLNX (L240,TITL) + 1
Cprh      TITL(LOC) = '&'
Cprh      CALL CVARAR (L21,HEADNG(7)(1:21),L21,TITL(LOC+1))
C
      XTYP = 3
      YTYP(1) = 2
      YTYP(2) = 0
      CALL GPLABL (XTYP, YTYP, ALEN, YLABL, YXLABL, BLNKLN, TITL)
C
      LOCLGD(1)= .05
      LOCLGD(2)= .95
      CALL GPLEDG (LOCLGD)
      DO 22 I= 1,NCRV
        CTYPE(I)   = 6
 22   CONTINUE
C
      NPLOT1 = 1
      NPLOT2 = NPLOT
Ckmf  write (99,*) ' full wrc curve'
Ckmf  write (99,*) '            nplot =', nplot
Ckmf  write (99,*) '              pp0 =', pp0
Ckmf  write (99,*) '              pp1 =', pp1
Ckmf  write (99,*) '  wrc frequency courve:   fcxpg   wrcfc   sysrfc'
      DO 17 I = 1,NPLOT
C       find last position to use for curve
Ckmf  write (99,3001) fcxpg(i), wrcfc(i), sysrfc(i)
 3001 format ( '                     ', 4f10.1)
        IF (FCXPG(I) .LT. PP0)   NPLOT2 = I
C       find first position to use for curve when log flow reasonable
        J = NPLOT + 1 - I
        IF (FCXPG(J) .GT. PP1 .AND. WRCFC(J) .GT. -1.0)   NPLOT1 = J
 17   CONTINUE
      PMIN = 1E20
      PMAX = -1E20
C
C     Save start/end plot positions for confidence limits
      NPLCL1 = NPLOT1
      NPLCL2 = NPLOT2
C
C     WRC frequency curve (solid line)
C
C     'WRC FREQUENCY CURVE '
      CALL CVARAR (L20,S3,L20,LBC(1,1))
      LST=NPLOT2-NPLOT1 + 1
Ckmf  write (99,*) ' '
Ckmf  write (99,*) '  ranges:  nplot1 =', nplot1
Ckmf  write (99,*) '           nplot2 =', nplot2
Ckmf  write (99,*) '            nplot =', nplot
Ckmf  write (99,*) '           npkplt =', npkplt
Ckmf  write (99,*) '  wrc frequency curve        yx    fcxpg   wrcfc'
      DO 35 I=NPLOT1,NPLOT2
        YX(I)=10.0**WRCFC(I)
        IF (YX(I) .GT. PMAX) PMAX = YX(I)
        IF (YX(I) .LT. PMIN) PMIN = YX(I)
Ckmf  write (99,3001) yx(i), fcxpg(i), wrcfc(i)
35    CONTINUE
      SYMBL(1) = 0
      LNTYP(1) = 1
      IDX = 1
      CALL GPDATA (IDX, LST, YX(NPLOT1), RETCOD)
      IDX = 2 
      CALL GPDATA (IDX, LST, FCXPG(NPLOT1), RETCOD)
C
C     observed peaks
C
C     'OBSERVED PEAKS      ' (plot o)
      CALL CVARAR (L20,S4,L20,LBC(1,2))
      LENOBS = 0
Ckmf  write (99,*) '  observed peaks             yx    syspp   pklog'
      DO 40 I=1,NPKPLT
        IF (SYSPP(I) .GT. -9.0 .AND. SYSPP(I) .LT. 9.0) THEN
          LENOBS = LENOBS + 1
          YX(I) = 10.0**PKLOG(I)
          IF (YX(I) .GT. PMAX) PMAX = YX(I)
          IF (YX(I) .LT. PMIN) PMIN = YX(I)
Ckmf  write (99,3001) yx(i), syspp(i), pklog(i)
        END IF
40    CONTINUE
      SYMBL(2) = 8
      LNTYP(2) = 0
      IDX = 3
      CALL GPDATA (IDX, NPKPLT, YX, RETCOD)
      IDX = 4    
      CALL GPDATA (IDX, NPKPLT, SYSPP, RETCOD)
C
C     systematic record
      NPLOT1 = 1
      NPLOT2 = NPLOT
      DO 45 I = 1, NPLOT
C       find last position to use for curve
        IF (FCXPG(I) .LT. PP0)   NPLOT2 = I
C       find first position to use for curve when log flow reasonable
        J = NPLOT + 1 - I
        IF (FCXPG(J) .GT. PP1 .AND. SYSRFC(J) .GT. -1.0)   NPLOT1 = J
 45   CONTINUE
      LST = NPLOT2 - NPLOT1 + 1
C
C     'SYSTEMATIC REC. FREQ' (dashed line)
      CALL CVARAR (L20,S6,L20,LBC(1,3))
Ckmf  write (99,*) '  systematic rec freq        yx    fcxpg   sysrfc'
      DO 50 I=NPLOT1,NPLOT2
        YX(I) = 10.0**SYSRFC(I)
        IF (YX(I) .GT. PMAX) PMAX = YX(I)
        IF (YX(I) .LT. PMIN) PMIN = YX(I)
Ckmf  write (99,3001) yx(i), fcxpg(i), sysrfc(i)
50    CONTINUE
      SYMBL(3) = 0
      LNTYP(3) = 2
      IDX = 5
      CALL GPDATA (IDX, LST, YX(NPLOT1), RETCOD)
      IDX = 6  
      CALL GPDATA (IDX, LST, FCXPG(NPLOT1), RETCOD)
C
      KNTCV = 3
C
C     Historic record
C
      IF (HSTFLG .EQ. 0) THEN
C       'HISTORICALLY ADJUST ' (plot x)
        KNTCV = KNTCV + 1
        CALL CVARAR (L20,S5,L20,LBC(1,KNTCV))
Ckmf  write (99,*) '  historically adjust        yx    wrcpp    pklog'
        DO 60 I=1,NPKPLT
          YX(I) = 10.0**PKLOG(I)
          IF (YX(I) .GT. PMAX) PMAX = YX(I)
          IF (YX(I) .LT. PMIN) PMIN = YX(I)
Ckmf  write (99,3001) yx(i), wrcpp(i), pklog(i)
60      CONTINUE
        SYMBL(KNTCV) = 2
        LNTYP(KNTCV) = 0
        IDX = IDX + 1
        CALL GPDATA (IDX, NPKPLT, YX, RETCOD)
        IDX = IDX + 1
        CALL GPDATA (IDX, NPKPLT, WRCPP, RETCOD)
      END IF
Ckmf  neither approach works, not sure what is wrong
Ckmf  may have to do with how data is shifted in arrays.
Ckmf  look at solving this at a later date for next release.
Ckmf  Nov 2000 - added option to plot confidence limits
Ckmf  IF (NOCLIM .NE. 1) THEN
C       lower and upper confidence limits
Ckmf  write (99,*) '  confidence limits low      yx    fcxpg    climl'
Ckmf    J = 0
Ckmf    DO 70 I = NPLOT1, NPLOT2
Ckmf      IF (CLIML(I) .GT. -9.  .AND.  CLIML(I) .LT. 9) THEN
C           good values
Ckmf  write (99,3001) yx(i), fcxpg(i), climl(i), climu(i)
Ckmf        J = J + 1
Ckmf        YX(J) = 10. ** CLIML(I)
Ckmf        IF (YX(J) .GT. PMAX) PMAX = YX(J)
Ckmf        IF (YX(J) .LT. PMIN) PMIN = YX(J)
Ckmf        YX(J+40) = 10. ** CLIMU(I)
Ckmf        IF (YX(J+40) .GT. PMAX) PMAX = YX(J+40)
Ckmf        IF (YX(J+40) .LT. PMIN) PMIN = YX(J+40)
Ckmf        YX(J+80) = FCXPG(I)
Ckmf      END IF
C70     CONTINUE
C       'confidence limits low' plot as dots
Ckmf    KNTCV = KNTCV + 1
Ckmf    CALL CVARAR ( L20, S7, L20, LBC(1,KNTCV) )
Ckmf    SYMBL(KNTCV) = 0
Ckmf    LNTYP(KNTCV) = 3
C       'confidence limits up' plot as dots
Ckmf    KNTCV = KNTCV + 1
Ckmf    CALL CVARAR ( L20, S8, L20, LBC(1,KNTCV) )
Ckmf    SYMBL(KNTCV) = 0
Ckmf    LNTYP(KNTCV) = 3
C       put data for both curves
Ckmf    IDX = IDX + 1
Ckmf    CALL GPDATA ( IDX, J, YX(1), RETCOD )
Ckmf    IDX = IDX + 1
Ckmf    CALL GPDATA ( IDX, J, YX(81), RETCOD )
Ckmf    IDX = IDX + 1
Ckmf    CALL GPDATA ( IDX, J, YX(41), RETCOD )
Ckmf    IDX = IDX + 1
Ckmf    CALL GPDATA ( IDX, J, YX(81), RETCOD )
Ckmf  END IF
Ckmf  save below for now
      IF (NOCLIM .NE. 1) THEN
C       'confidence limits low' plot as dots
        KNTCV = KNTCV + 1
        CALL CVARAR ( L20, S7, L20, LBC(1,KNTCV) )
Ckmf      write (99,*) '  confidence limits low      yx    fcxpg    climl'
        DO 70 I = NPLCL1, NPLCL2
Ckmf      write (99,*) '  I,CLIML(I) ',I,CLIML(I)
          IF (CLIML(I) .GT. -9.  .AND.  CLIML(I) .LT. 9) THEN
            YX(I) = 10. ** CLIML(I)
            IF (YX(I) .GT. PMAX) PMAX = YX(I)
            IF (YX(I) .LT. PMIN) PMIN = YX(I)
Ckmf      write (99,3001) yx(i), fcxpg(i), climl(i)
          END IF
 70     CONTINUE
        SYMBL(KNTCV) = 0
        LNTYP(KNTCV) = 3
        COLOR(KNTCV) = 11
        LST = NPLCL2 - NPLCL1 + 1
        IDX = IDX + 1
        CALL GPDATA ( IDX, LST, YX(NPLCL1), RETCOD )
        IDX = IDX + 1
        CALL GPDATA ( IDX, LST, FCXPG(NPLCL1), RETCOD )
C       'confidence limits up' plot as dots
        KNTCV = KNTCV + 1
        CALL CVARAR ( L20, S8, L20, LBC(1,KNTCV) )
Ckmf      write (99,*) '  confidence limits up       yx    fcxpg    climu'
        DO 80 I = NPLCL1, NPLCL2
          IF (CLIMU(I) .GT. -9.  .AND.  CLIMU(I) .LT. 9) THEN
            YX(I) = 10. ** CLIMU(I)
            IF (YX(I) .GT. PMAX) PMAX = YX(I)
            IF (YX(I) .LT. PMIN) PMIN = YX(I)
Ckmf      write (99,3001) yx(i), fcxpg(i), climu(i)
          END IF
 80     CONTINUE
        SYMBL(KNTCV) = 0
        LNTYP(KNTCV) = 3
        COLOR(KNTCV) = 11
        IDX = IDX + 1
        CALL GPDATA ( IDX, LST, YX(NPLCL1), RETCOD )
        IDX = IDX + 1
        CALL GPDATA ( IDX, LST, FCXPG(NPLCL1), RETCOD )
      END IF
Ckmf  Nov 2000 - end of new code for confidence limits
C
C     set specs for each variable
Ckmf  vmin and vmax are never initialized               <-------
      CALL GPVAR (VMIN,VMAX,WHICH,TRANSF,LBV)  
C
C     set which variable for which curve
      CALL GPWCXY (L1,L1,L2)
      CALL GPWCXY (L2,L3,L4)
      CALL GPWCXY (L3,L5,L6)
      CALL GPWCXY (L4,L7,L8)
      CALL GPWCXY (L5,L9,L10)
      CALL GPWCXY (L6,L11,L12)
C
C     set specs for curves
      CALL GPCURV (CTYPE, LNTYP, SYMBL, COLOR, PATTRN, LBC)
C
C     set the min and max for the plot
      CALL SCALIT(YTYP(1),PMIN,PMAX,PLMN(1),PLMX(1))
      PLMX(4)=3.0
      PLMN(4)=-3.0
      CALL GPSCLE (PLMN, PLMX, TICS, BVFLG)
C
C     set plot sizes
      SIZEL= 0.11
      XPAGE= 10.0
      YPAGE= 8.0
      XPHYS= 1.5
      YPHYS= 1.5
      XLEN = 7.0
      YLEN = 5.0
      CALL GPSIZE (SIZEL, XPAGE, YPAGE, XPHYS, YPHYS, XLEN, YLEN,
     I             ALEN)
C
C     put notice
      FYT = 8.0*SIZEL/YLEN
      FXT = ((36.0+7.0)*SIZEL*0.7)/XLEN
      CPR = 30
      NCHR = 120
C     include date in first line of additional text
      LSTR = TRIM(HEADNG(4))
      I = LEN(LSTR)
      IF (I.GT.16) THEN
        C0(14:29) = LSTR(I-15:I)
      END IF
Cprh      C0(14:30) = HEADNG(7)(1:17)
      CALL CVARAR (L30,C0,L30,CTXT(1))
      CALL CVARAR (L30,C1,L30,CTXT(31))
      CALL CVARAR (L30,C2,L30,CTXT(61))
      CALL CVARAR (L30,C3,L30,CTXT(91))
      CALL GPTEXT (NCHR, CPR, FYT, FXT, CTXT)
C
C     do the plot
      WSID = 1      
      CALL PSTUPW (WSID, RETCOD)
      CALL PLTONE
C     exit from plot, dont wait
      IWAIT = 0
      CALL ANPRGT (L1, CMPTYP)
      IF (CMPTYP .NE. 1) THEN 
C       dont close workstation
        ICLOS = 0
      ELSE
C       close workstation on pc
        ICLOS = 1
      END IF
      CALL PDNPLT (WSID,ICLOS,IWAIT)
      WRITE(INDSTR,'(I3)') PLTIND
      IF (PLTIND.LT.10) THEN
        FNAME = 'PKFQ-'//INDSTR(3:3)//'.'//GFORM
      ELSE IF (PLTIND.LT.100) THEN
        FNAME = 'PKFQ-'//INDSTR(2:3)//'.'//GFORM
      ELSE
        FNAME = 'PKFQ-'//INDSTR//'.'//GFORM
      END IF
      CALL IOsDeleteFile(FNAME)
      CALL IOsRenameFile('INTERACT.'//GFORM, FNAME)
C
      RETURN
      END

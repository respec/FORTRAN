C
C
C
      SUBROUTINE   GPUTVL
     I                   (IVAL, RVAL, CVAL)
C
C     + + + PURPOSE + + +
C     Places values passed in via input arrays into CPLTV and CPLTC 
C     common blocks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IVAL(265)
      REAL         RVAL(64)
      CHARACTER*1  CVAL(1400)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - array containing values for all integer variables in CPLOT
C     RVAL   - array containing values for all real variables in CPLOT
C     CVAL   - array containing values for all character variables in CPLOT
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4, L6, L80, L120, L240, ICNTR, RCNTR, CHCNTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI, COPYR, COPYC
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      L6 = 6
      L80 = 80
      L120 = 120
      L240 = 240
C
      DEVCOD = IVAL(1)
      DEVTYP = IVAL(2)
      FE = IVAL(3)
      NCRV = IVAL(4)
      NVAR = IVAL(5)
C
      CALL COPYR (L4, RVAL, PLMX)
      CALL COPYR (L4, RVAL(5), PLMN)
      CALL COPYR (TSMAX, RVAL(9), YMIN)
      RCNTR = 9 + TSMAX
      CALL COPYR (TSMAX, RVAL(RCNTR), YMAX) 
      RCNTR = RCNTR + TSMAX
C
      CALL COPYI (L4, IVAL(6), TICS)
      XTYPE = IVAL(10)
      YTYPE(1) = IVAL(11)
      YTYPE(2) = IVAL(12)
      CALL COPYI (TSMAX, IVAL(13), WHICH)
      ICNTR = 13 + TSMAX
      CALL COPYI (3*TSMAX, IVAL(ICNTR), WCHVR)
      ICNTR = ICNTR + 3*TSMAX
      CALL COPYI (TSMAX, IVAL(ICNTR), CTYPE)
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, IVAL(ICNTR), DTYPE)
      ICNTR = ICNTR + TSMAX
C
      CALL COPYC (L240, CVAL, TITL)
      CALL COPYC (L80, CVAL(241), YLABL)
      CALL COPYC (L80, CVAL(321), YXLABL)
      CALL COPYC (L80, CVAL(401), XLABL)
      CALL COPYC (L80, CVAL(481), YALABL)
      CALL COPYC (20*TSMAX, CVAL(561), LBV)
      CHCNTR = 561 + 20*TSMAX
      CALL COPYC (20*TSMAX, CVAL(CHCNTR), LBC)
      CHCNTR = CHCNTR + 20*TSMAX
C
      CALL COPYI (TSMAX, IVAL(ICNTR), TSTEP)
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, IVAL(ICNTR), TUNITS)
      ICNTR = ICNTR + TSMAX
      CALL COPYI (L6, IVAL(ICNTR), SDATIM)
      ICNTR = ICNTR + 6
      CALL COPYI (L6, IVAL(ICNTR), EDATIM)
      ICNTR = ICNTR + 6
      CALL COPYI (TSMAX, IVAL(ICNTR), SYMBL)
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, IVAL(ICNTR), LNTYP)
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, IVAL(ICNTR), PATTRN)
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, IVAL(ICNTR), COLOR)
      ICNTR = ICNTR + TSMAX
C
      YLEN = RVAL(RCNTR)
      XLEN = RVAL(RCNTR + 1)
      ALEN = RVAL(RCNTR + 2)
      YPAGE = RVAL(RCNTR + 3)
      XPAGE = RVAL(RCNTR + 4)
      XPHYS = RVAL(RCNTR + 5)
      YPHYS = RVAL(RCNTR + 6)
      SIZEL = RVAL(RCNTR + 7)
      LOCLGD(1) = RVAL(RCNTR + 8)
      LOCLGD(2) = RVAL(RCNTR + 9)
      RCNTR = RCNTR + 10
C
      CALL COPYI (TSMAX, IVAL(ICNTR), TRANSF) 
      ICNTR = ICNTR + TSMAX
C
      BVALFG(1) = IVAL(ICNTR)
      BVALFG(2) = IVAL(ICNTR + 1)
      BVALFG(3) = IVAL(ICNTR + 2)
      BVALFG(4) = IVAL(ICNTR + 3)
      ICNTR = ICNTR + 4
C
      BLNKIT(1) = RVAL(RCNTR)
      BLNKIT(2) = RVAL(RCNTR + 1)
      BLNKIT(3) = RVAL(RCNTR + 2)
      BLNKIT(4) = RVAL(RCNTR + 3)
      RCNTR = RCNTR + 4
C
      CALL COPYC (L120, CVAL(CHCNTR), CTXT)
C
      CPR = IVAL(ICNTR)
      NCHR = IVAL(ICNTR + 1)
      MTPLUT = IVAL(ICNTR + 2)
C
      FYT = RVAL(RCNTR)
      FXT = RVAL(RCNTR + 1)
      CALL COPYR (L4, RVAL(RCNTR + 2), XWINLC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGETVL
     O                   (IVAL, RVAL, CVAL)
C
C     + + + PURPOSE + + +
C     Places values from CPLTV and CPLTC common blocks in output arrays.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IVAL(265)
      REAL         RVAL(64)
      CHARACTER*1  CVAL(1400)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - array containing values for all integer variables in CPLOT
C     RVAL   - array containing values for all real variables in CPLOT
C     CVAL   - array containing values for all character variables in CPLOT
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4, L6, L80, L120, L240, ICNTR, RCNTR, CHCNTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI, COPYR, COPYC
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      L6 = 6
      L80 = 80
      L120 = 120
      L240 = 240
C
      IVAL(1) = DEVCOD  
      IVAL(2) = DEVTYP 
      IVAL(3) =  FE 
      IVAL(4) = NCRV 
      IVAL(5) = NVAR 
C
      CALL COPYR (L4, PLMX, RVAL)
      CALL COPYR (L4, PLMN, RVAL(5))
      CALL COPYR (TSMAX, YMIN, RVAL(9))
      RCNTR = 9 + TSMAX
      CALL COPYR (TSMAX, YMAX, RVAL(RCNTR)) 
      RCNTR = RCNTR + TSMAX
C
      CALL COPYI (L4, TICS, IVAL(6))
      IVAL(10) = XTYPE 
      IVAL(11) = YTYPE(1) 
      YTYPE(2) = IVAL(12)
      CALL COPYI (TSMAX, WHICH, IVAL(13))
      ICNTR = 13 + TSMAX
      CALL COPYI (3*TSMAX, WCHVR, IVAL(ICNTR))
      ICNTR = ICNTR + 3*TSMAX
      CALL COPYI (TSMAX, CTYPE, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, DTYPE, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
C
      CALL COPYC (L240, TITL, CVAL)
      CALL COPYC (L80, YLABL, CVAL(241))
      CALL COPYC (L80, YXLABL, CVAL(321))
      CALL COPYC (L80, XLABL, CVAL(401))
      CALL COPYC (L80, YALABL, CVAL(481))
      CALL COPYC (20*TSMAX, LBV, CVAL(561))
      CHCNTR = 561 + 20*TSMAX
      CALL COPYC (20*TSMAX, LBC, CVAL(CHCNTR))
      CHCNTR = CHCNTR + 20*TSMAX
C
      CALL COPYI (TSMAX, TSTEP, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, TUNITS, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
      CALL COPYI (L6, SDATIM, IVAL(ICNTR))
      ICNTR = ICNTR + 6
      CALL COPYI (L6, EDATIM, IVAL(ICNTR))
      ICNTR = ICNTR + 6
      CALL COPYI (TSMAX, SYMBL, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, LNTYP, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, PATTRN, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
      CALL COPYI (TSMAX, COLOR, IVAL(ICNTR))
      ICNTR = ICNTR + TSMAX
C
      RVAL(RCNTR) = YLEN 
      RVAL(RCNTR + 1) = XLEN 
      RVAL(RCNTR + 2) = ALEN 
      RVAL(RCNTR + 3) = YPAGE 
      RVAL(RCNTR + 4) = XPAGE 
      RVAL(RCNTR + 5) = XPHYS 
      RVAL(RCNTR + 6) = YPHYS 
      RVAL(RCNTR + 7) = SIZEL 
      RVAL(RCNTR + 8) = LOCLGD(1) 
      RVAL(RCNTR + 9) = LOCLGD(2) 
      RCNTR = RCNTR + 10
C
      CALL COPYI (TSMAX, TRANSF, IVAL(ICNTR)) 
      ICNTR = ICNTR + TSMAX
C
      IVAL(ICNTR) = BVALFG(1) 
      IVAL(ICNTR + 1) = BVALFG(2) 
      IVAL(ICNTR + 2) = BVALFG(3) 
      IVAL(ICNTR + 3) = BVALFG(4) 
      ICNTR = ICNTR + 4
C
      RVAL(RCNTR) = BLNKIT(1) 
      RVAL(RCNTR + 1) = BLNKIT(2) 
      RVAL(RCNTR + 2) = BLNKIT(3) 
      RVAL(RCNTR + 3) = BLNKIT(4) 
      RCNTR = RCNTR + 4
C
      CALL COPYC (L120, CTXT, CVAL(CHCNTR))
C
      IVAL(ICNTR) = CPR 
      IVAL(ICNTR + 1) = NCHR 
      IVAL(ICNTR + 2) = MTPLUT 
C
      RVAL(RCNTR) = FYT 
      RVAL(RCNTR + 1) = FXT 
      CALL COPYR (L4, XWINLC, RVAL(RCNTR + 2))
C
      RETURN
      END
